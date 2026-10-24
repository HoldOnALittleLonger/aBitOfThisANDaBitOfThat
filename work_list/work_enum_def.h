#ifndef _WORK_ENUM_DEF_H_
#define _WORK_ENUM_DEF_H_

#include <stddef.h>

/**
 * work_thread_num - worker identifier
 * # if a work is not in progress,its worker
 *   should be set to WTHREAD_UNSPECIFIED
 */
enum work_thread_num {
        WTHREAD0,
        WTHREAD1,
        WTHREAD2,
        WTHREAD3,
        WTHREAD_MAX_NUM,
        WTHREAD_UNSPECIFIED = WTHREAD_MAX_NUM,
};

/**
 * work_state - state of a work
 */
enum work_state {
        WORK_STATE_INIT,
        WORK_STATE_INSCHED,
        WORK_STATE_INPROGRESS,
        WORK_STATE_FINISHED,
};

#endif
