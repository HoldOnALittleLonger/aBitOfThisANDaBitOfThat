#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>

#include "work_list.h"
#include "work.h"

int handler(void *arg)
{
        printf("work handler is called\n");
        printf("work handler argument is %p\n", arg);
        printf("work handler sleep 1 second\n");
        sleep(1);
        printf("work handler return\n");
        return 0;
}

void finish_handler(int ret, struct work_struct *work)
{
        printf("work finish handler is called\n");
        printf("work handler returned -- %d\n", ret);
        printf("worker is %d\n", work->wthread_n);
        printf("work state is %d\n", work->state);
        printf("work refcnt is %d\n", work->refcnt);
        printf("work descriptor at %p\n", work);
        printf("work finish handler sleep 1 second\n");
        sleep(1);
        printf("work finish handler wakeup,return\n");
}

#define WORK_NUM 5

int main(void)
{
        struct work_list *wl = NULL;
        struct work_struct *w_desc[WORK_NUM] = {NULL};

        printf("main thread: allocate work list\n");
        wl = alloc_work_list();
        if (!wl) {
                fprintf(stderr, "main thread: failed to allocate work list,exit\n");
                exit(EXIT_FAILURE);
        }

        printf("main thread: create workers\n");
        if (create_workers(wl) < 0) {
                fprintf(stderr, "start workers failed,exit\n");
                exit(EXIT_FAILURE);
        }        

        printf("main thread: allocate work descriptors and register them\n");
        for (unsigned short idx = 0; idx < WORK_NUM; ++idx) {
                struct work_struct *w = alloc_work(handler, finish_handler, NULL);
                if (w) {
                        printf("main thread: allocated work -- %p\n", w);
                        w_desc[idx] = w;
                        printf("main thread: register work -- %p\n", w);
                        if (register_work(w, wl) < 0) {
                                fprintf(stderr, "main thread: register work failed,exit\n");
                                exit(EXIT_FAILURE);
                        }
                        else
                                printf("main thread: registered work -- %p\n", w);
                                
                } else {
                        fprintf(stderr, "main thread: failed to allocate work,exit\n");
                        exit(EXIT_FAILURE);
                }
        }

        printf("main thead: poll works whether been finished...\n");
        for (unsigned short widx = 0; widx < WORK_NUM; ++widx) {
                struct work_struct *w = w_desc[widx];
                do {
                        if (work_is_finished(w))
                                break;
                } while (1);
        }

        printf("main thread: all works finished,put them\n");
        for (unsigned short widx = 0; widx < WORK_NUM; ++widx)
                work_put(w_desc[widx]);

        printf("main thread: stop workers\n");
        stop_all_workers(wl);

        printf("main thread: put work list\n");
        work_list_put(wl);

        printf("main thread: process exit\n");
        
        exit(EXIT_SUCCESS);
}
