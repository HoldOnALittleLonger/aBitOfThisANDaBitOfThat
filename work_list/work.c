#include <stdlib.h>

#include "work.h"

static void BUG(const char *arg) { }

/**
 * default_work_destructor - dtor of work descriptor
 * @work:                    the work
 * # just destroy work's spinlock,and free its memory
 */
static void default_work_destructor(struct work_struct *work)
{
        acquire_work_lock(work);
        if (work->refcnt > 0) {
                BUG("destroy a work whose refcnt is not 0.");
                release_work_lock(work);
                return;
        }
        release_work_lock(work);
        pthread_spin_destroy(&work->lock);
        free(work);
}

/**
 * allocate and initialize a new work,return it back to
 * caller with ->refct set to 1.
 */
struct work_struct *alloc_work(work_handler_t wh,
                               work_finish_handler_t wfh,
                               void *arg)
{
        struct work_struct *new_work = NULL;
        if (!wh || !wfh)
                return NULL;

        new_work = malloc(sizeof(struct work_struct));
        if (!new_work)
                return NULL;

        /* initialization */
        new_work->handler = wh;
        new_work->finish_handler = wfh;
        new_work->data = arg;
        new_work->residing_list = NULL;
        list_head_init(&new_work->list);
        new_work->wthread_n = WTHREAD_UNSPECIFIED;        
        new_work->state = WORK_STATE_INIT;
        new_work->destructor = default_work_destructor;
        
        if (pthread_spin_init(&new_work->lock, PTHREAD_PROCESS_PRIVATE)
            < 0) {
                BUG("init work's spinlock failed.");
                goto exit_free;
        }
        new_work->refcnt = 1;
        return new_work;

exit_free:
        free(new_work);
        return NULL;
}
