#ifndef _WORK_LIST_H_
#define _WORK_LIST_H_

#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>

#include "list_head.h"
#include "work_enum_def.h"

struct work_struct;

/**
 * work_list - work list descriptor
 * @workers:   array to saves workers' TIDs
 * @works:     list of registered works
 * @refcnt:    refcnt of this work_list
 * @lock:      concurrency protection,for
 *             above fields
 * @is_available:
 *             indicates whether there is a work
 *             available to worker
 * @cond_mutex:
 *             mutex for condition variable and
 *             @is_available
 * @cond:      condition variable that workers will
 *             wait on
 */
struct work_list {
        pthread_t workers[WTHREAD_MAX_NUM];        
        struct list_head works;        
        int refcnt;        
        pthread_spinlock_t lock;

        struct {
                bool is_available;
                pthread_mutex_t cond_mutex;
                pthread_cond_t cond;
        } work_available;        
};

/**
 * work_list_lock - lock work list prevent concurreny accessing
 */
static inline void work_list_lock(struct work_list *work_list)
{
        pthread_spin_lock(&work_list->lock);
}

/**
 * work_list_unlock - unlock work list
 */
static inline void work_list_unlock(struct work_list *work_list)
{
        pthread_spin_unlock(&work_list->lock);
}

extern struct work_list *alloc_work_list(void);
extern void work_list_dtor(struct work_list *work_list);

/**
 * get refcnt unless refcnt is _zero_.
 */
static inline bool work_list_get(struct work_list *work_list)
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
static inline void work_list_put(struct work_list *work_list)
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

/**
 * which_wthread - function,maps TID to worker identifier
 */
static inline enum work_thread_num which_wthread(struct work_list *work_list)
{
        unsigned short idx = 0;
        pthread_t selfid = pthread_self();

        work_list_lock(work_list);
        for (; idx < WTHREAD_MAX_NUM; ++idx)
                if (selfid == work_list->workers[idx])
                        break;
        work_list_unlock(work_list);
        
        return (enum work_thread_num)idx;
}

extern int create_workers(struct work_list *work_list);
extern void stop_all_workers(struct work_list *work_list);

extern int register_work(struct work_struct *work, struct work_list *work_list);
extern void unregister_work(struct work_struct *work);

#endif
