#include <errno.h>
#include <stdlib.h>

#include "work_list.h"
#include "work.h"

/* un-implemented */
static void BUG(const char *arg) { }

/**
 * alloc_work_list - allocate a work list
 * return:           work_list descriptor OR NULL
 * # this routine allocate and initializes a new work list,
 *   if all succeed,->refcnt is set to 1.
 * # mutex and condition variable are initialized with default
 *   attributes
 */
struct work_list *alloc_work_list(void)
{
        struct work_list *wl = NULL;
        pthread_mutexattr_t mutex_attr;
        
        wl = malloc(sizeof(struct work_list));
        if (!wl)
                return wl;

        if (pthread_spin_init(&wl->lock, PTHREAD_PROCESS_PRIVATE) < 0)
                goto exit_err_free;        

        list_head_init(&wl->works);

        wl->work_available.is_available = false;

        /* we use error checking mutex for cancelation */
        pthread_mutexattr_init(&mutex_attr);
        pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_ERRORCHECK);
        pthread_mutex_init(&wl->work_available.cond_mutex, &mutex_attr);
        
        pthread_cond_init(&wl->work_available.cond, NULL);

        /* set all records of TIDs to 0 */
        for (unsigned short idx = 0; idx < WTHREAD_MAX_NUM; ++idx)
                wl->workers[idx] = 0;

        wl->refcnt = 1;
        return wl;

exit_err_free:
        free(wl);
        return NULL;
}

/**
 * work_list_dtor - default destructor for work_list
 * @work_list:      the work list
 * # has effect only when refcnt is 0
 * # destroy mutex,condition variable,and spinlock,
 *   finally free memory
 */
void work_list_dtor(struct work_list *work_list)
{
        work_list_lock(work_list);
        if (work_list->refcnt > 0) {
                BUG("dtor on object refcnt > 0");
                work_list_unlock(work_list);
                return;
        }
        work_list_unlock(work_list);

        /* we do not check pthread whether been stopped */

        pthread_mutex_destroy(&work_list->work_available.cond_mutex);
        pthread_cond_destroy(&work_list->work_available.cond);
        pthread_spin_destroy(&work_list->lock);
        
        free(work_list);
}

/**
 * get refcnt unless refcnt is _zero_.
 */
bool work_list_get(struct work_list *work_list)
{
        work_list_lock(work_list);
        if (work_list->refcnt <= 0) {
                work_list_unlock(work_list);
                return false;
        }
        work_list->refcnt++;
        work_list_unlock(work_list);
        return true;
}

/**
 * put refcnt,automatically destroy if refcnt falls into _zero_.
 */
void work_list_put(struct work_list *work_list)
{
        work_list_lock(work_list);
        if (work_list->refcnt > 0) {
                work_list->refcnt--;
                if (!work_list->refcnt) {
                        work_list_unlock(work_list);
                        work_list_dtor(work_list);
                        return;
                }
        }
        work_list_unlock(work_list);        
}

void *worker_routine(void *arg);

/**
 * create_workers - create all worker threads
 * @work_list:      the work list
 * return:          0 OR -1
 * # hold work_list refcnt,and start workers,saves their TIDs
 *   into ->workers[].
 *   if any one thread is failed to starts,UNDO been started
 *   threads -- cancel them and wait them
 * # this routine can only be called by main thread
 */
int create_workers(struct work_list *work_list)
{
        if (!work_list_get(work_list))
                return -1;

        for (unsigned short idx = 0; idx < WTHREAD_MAX_NUM; ++idx) {
                pthread_t tid = 0;
                if (pthread_create(&tid, NULL, worker_routine, work_list) < 0)
                        goto exit_undo_creation;

                work_list_lock(work_list);
                work_list->workers[idx] = tid;
                work_list_unlock(work_list);                
        }

        work_list_put(work_list);
        return 0;

exit_undo_creation:
        stop_all_workers(work_list);
        work_list_put(work_list);
        return -1;
}

/**
 * stop_all_workers - stop all started threads
 * @work_list:        work list
 * # stop the threads their TIDs is stored in ->workers[],
 *   and the TID cant be _zero_
 * # after the thread jointed into main thread,reset
 *   its recorder
 */
void stop_all_workers(struct work_list *work_list)
{
        if (!work_list_get(work_list))
                return;

        for (unsigned short idx = 0; idx < WTHREAD_MAX_NUM; ++idx) {
                pthread_t tid = 0;
                work_list_lock(work_list);
                tid = work_list->workers[idx];
                work_list_unlock(work_list);

                if (tid) {
                        pthread_cancel(tid);
                        pthread_join(tid, NULL);
                        work_list_lock(work_list);
                        work_list->workers[idx] = 0; /* reset tid */
                        work_list_unlock(work_list);
                }
        }        
        
        work_list_put(work_list);
}

/**
 * register a new work to the work_list.
 * if succeed,both refcnt(s) of new work and work_list are increase 1
 * this increment can be UNDO through unregister_work() routine.
 * ->work_available.is_bool will be set to TRUE if we succeed,and
 * this event would be signal to waiters,to restart just one worker
 * thread that is waiting on ->worker_available.cond .
 * # do not register the same work more than once,because its refcnt
 *   would be got by work_list,and this routine does not check whether
 *   the work been registered.
 */
int register_work(struct work_struct *work, struct work_list *work_list)
{
        if (!work_list_get(work_list))
                return -1;
        if (!work_get(work))
                goto exit_err_put_work_list;

        work_list_lock(work_list);
        list_add(&work->list, &work_list->works);
        work_list_unlock(work_list);

        acquire_work_lock(work);
        work->residing_list = work_list;
        release_work_lock(work);
        modify_work_state(work, WORK_STATE_INSCHED);

        /* signal waiters */
        pthread_mutex_lock(&work_list->work_available.cond_mutex);
        work_list->work_available.is_available = true;
        pthread_cond_signal(&work_list->work_available.cond);
        pthread_mutex_unlock(&work_list->work_available.cond_mutex);        
        
        return 0;

exit_err_put_work_list:
        work_list_put(work_list);
        return -1;
}

/**
 * unregister a work from work_list.
 * this work's state cant be WORK_STATE_INIT.
 * UNDO increment of refcnt(s) executed in register_work().
 * this routine would not products condition event.
 */
void unregister_work(struct work_struct *work)
{
        struct work_list *wl = NULL;
        enum work_state state;

        acquire_work_lock(work);
        wl = work->residing_list;
        state = work->state;
        release_work_lock(work);

        /* have not been registered */
        if (state == WORK_STATE_INIT || !wl)
                return;

        work_list_lock(wl);
        list_del(&work->list);
        work_list_unlock(wl);

        acquire_work_lock(work);
        work->residing_list = NULL;
        release_work_lock(work);

        work_put(work);
        work_list_put(wl);
}

/**
 * return next work from work_list,may sleep.
 * this routine will changes @is_available to false
 * if there is no pending works,otherwise,it will
 * notifies waiters there is works to do.
 * # this routine can only be called by thread function.
 * # this encapsulation is used to separates logical
 *   about get new work from work thread function.
 */
static struct work_struct *get_next_work(struct work_list *work_list)
{
        struct work_struct *w = NULL, *iter = NULL;
        int pending_works = 0;

        /* wait new work */
        pthread_mutex_lock(&work_list->work_available.cond_mutex);
        while (!work_list->work_available.is_available)
                pthread_cond_wait(&work_list->work_available.cond,
                                  &work_list->work_available.cond_mutex);
        pthread_mutex_unlock(&work_list->work_available.cond_mutex);

        /**
         * traverse work list to find work.
         * this operation is time-cost in the case the list
         * is too long,the better way is to use rw lock instead
         * spinlock.
         */
        work_list_lock(work_list);
        list_for_each_entry(iter, &work_list->works, list) {
                if (work_is_sched(iter)) {
                        ++pending_works;
                        if (!w) {
                                w = iter;
                                --pending_works; /* we get one */
                        }
                }
        }
        work_list_unlock(work_list);

        /* we will return this work,so modify its state */
        if (w) {
                enum work_thread_num n;
                n = which_wthread(work_list);
                modify_work_state(w, WORK_STATE_INPROGRESS);
                acquire_work_lock(w);
                w->wthread_n = n;
                release_work_lock(w);
        }

        if (pending_works) {
                /**
                 * there is pending works,we need to signal waiter
                 * about this info.
                 */
                pthread_mutex_lock(&work_list->work_available.cond_mutex);
                work_list->work_available.is_available = true;
                pthread_cond_signal(&work_list->work_available.cond);
                pthread_mutex_unlock(&work_list->work_available.cond_mutex);
        } else {
                /**
                 * there is no pending works,we just reset cond to false,
                 * without to signal waiters.
                 */
                pthread_mutex_lock(&work_list->work_available.cond_mutex);
                work_list->work_available.is_available = false;
                pthread_mutex_unlock(&work_list->work_available.cond_mutex);
        }
        
        return w;
}

/* two wrappers for work handling */
static inline int handle_work(struct work_struct *work)
{
        return work->handler(work->data);
}

static inline void finish_work(int result, struct work_struct *work)
{
        work->finish_handler(result, work);
        modify_work_state(work, WORK_STATE_FINISHED);
        acquire_work_lock(work);
        work->wthread_n = WTHREAD_UNSPECIFIED;
        release_work_lock(work);
}

static void wthread_cleanup(void *work_list)
{
        /* just release refcounter */
        struct work_list *wl = (struct work_list *)work_list;

        /**
         * we can unlock the mutex,because its type is error checking.
         * if the cancelation point is not cond wait,the mutex may
         * not be acquired by this thread,so unlock it will returns
         * EPERM,we ignore the error.
         * otherwise,the lock is acquired by this thread,we unlock it.
         */
        pthread_mutex_unlock(&wl->work_available.cond_mutex);
        
        work_list_put(wl);
}

/**
 * called by thread function if it failed to get work's refcnt
 * when it going to handle the work.
 * this routine will rollback work's state from WORK_STATE_INPROGRESS
 * to WORK_STATE_INSCHED,and signal waiters of the condition.
 */
static void worker_rollback_work(struct work_struct *work, struct work_list *work_list)
{
        /* rollback work's state */
        modify_work_state(work, WORK_STATE_INSCHED);
        acquire_work_lock(work);
        work->wthread_n = WTHREAD_UNSPECIFIED;
        release_work_lock(work);

        /* signal waiters */
        pthread_mutex_lock(&work_list->work_available.cond_mutex);
        work_list->work_available.is_available = true;
        pthread_cond_signal(&work_list->work_available.cond);
        pthread_mutex_unlock(&work_list->work_available.cond_mutex);
}

void *worker_routine(void *arg)
{
        struct work_list *work_list = (struct work_list *)arg;

        /**
         * we wont the work_list is destroyed before its workers
         * end.
         */
        if (!work_list_get(work_list))
                pthread_exit((void *)EFAULT);

        pthread_cleanup_push(wthread_cleanup, work_list);

        /**
         * this thread exits only when cancel is required.
         */
        do {
                int ret = 0;
                struct work_struct *work = get_next_work(work_list);

                if (!work)
                        continue;

                /**
                 * we get the refcnt of work because we are going
                 * to handle it.
                 */
                if (work_get(work)) {
                        ret = handle_work(work);
                        finish_work(ret, work);
                        work_put(work);
                        unregister_work(work);
                }
                else
                        worker_rollback_work(work, work_list);                        
                        /**
                         * we get the refcnt failed,so we rollback
                         * its state to in-sched,and signal waiter
                         * to retry this work.
                         * # actually,work_get() only fails when
                         *   work's refcnt drops to _zero_,in this
                         *   case,the work is unavailable.
                         *   but register_work() has got refcnt of
                         *   the work,thus,at there,its refcnt
                         *   at least is 1.if its refcnt is 0,
                         *   that means some control path incorrectly
                         *   has put it.
                         */                        
        } while (1);

        pthread_cleanup_pop(1);
        
        return NULL;
}
