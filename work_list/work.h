#ifndef _WORK_H_
#define _WORK_H_

#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>

#include "list_head.h"
#include "work_enum_def.h"

struct work_struct;
struct work_list;

/**
 * type-definition for work handler
 */
typedef int (*work_handler_t)(void *);

/**
 * type-definition for finish handler of work
 */
typedef void (*work_finish_handler_t)(int, struct work_struct *);

/**
 * work_struct - work descriptor
 * @handler:     handler for this work
 * @finish_handler:
 *               finish handler for this work when handler
 *               has accomplished
 * @data:        argument to @handler
 * @lock:        lock for concurrency accessing
 * @refcnt:      refcnt of this work
 * @wthread_n:   worker identifier for the worker that
 *               is taking charge of this work
 * @state:       work state
 * @residing_list:
 *               pointer to work_list on which this work
 *               registered with
 * @list:        list linker used to link this work into
 *               work_list
 *               # this field has no concurrency protector,
 *                 instead,the whole work_list->works is
 *                 protected by a lock.if we need to modify
 *                 this @list,we need that lock,actually,
 *                 this field only be modified by work_list's
 *                 routines except the initialization time
 * @destructor:  dtor of this work
 */
struct work_struct {
        work_handler_t handler;
        work_finish_handler_t finish_handler;
        void *data;

        pthread_spinlock_t lock;
        int refcnt;
        enum work_thread_num wthread_n;
        enum work_state state;
        struct work_list *residing_list;        

        struct list_head list;
        
        void (*destructor)(struct work_struct *work);
};

/* acquire_work_lock - lock work to prevent race condition */
static inline void acquire_work_lock(struct work_struct *work)
{
        pthread_spin_lock(&work->lock);
}

/* release_work_lock - unlock work */
static inline void release_work_lock(struct work_struct *work)
{
        pthread_spin_unlock(&work->lock);
}

extern bool work_get(struct work_struct *);
extern void work_put(struct work_struct *);

/* modify work's state */
static inline void modify_work_state(struct work_struct *work, enum work_state new_state)
{
        acquire_work_lock(work);
        work->state = new_state;
        release_work_lock(work);
}

extern enum work_state __return_work_state(struct work_struct *);

/* test whether work been initialized */
static inline bool work_is_init(struct work_struct *work)
{
        return __return_work_state(work) == WORK_STATE_INIT;
}

/* test whether work been registered */
static inline bool work_is_sched(struct work_struct *work)
{
        return __return_work_state(work) == WORK_STATE_INSCHED;
}

/* test whether work been handling by a worker */
static inline bool work_is_inprogress(struct work_struct *work)
{
        return __return_work_state(work) == WORK_STATE_INPROGRESS;
}

/* test whether work been finished */
static inline bool work_is_finished(struct work_struct *work)
{
        return __return_work_state(work) == WORK_STATE_FINISHED;
}

/* return identifier of worker of this work */
static inline enum work_thread_num work_threadn(struct work_struct *work)
{
        enum work_thread_num n;
        acquire_work_lock(work);
        n = work->wthread_n;
        release_work_lock(work);
        return n;
}

extern struct work_struct *alloc_work(work_handler_t wh,
                                      work_finish_handler_t wfh,
                                      void *arg);

#endif
