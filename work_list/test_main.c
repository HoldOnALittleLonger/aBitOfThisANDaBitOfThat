#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>

#include "work_list.h"
#include "work.h"

int handler(void *arg)
{
        sleep(1);
        return 0;
}

void finish_handler(int ret, struct work_struct *work)
{
        sleep(1);
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

        printf("main thread: poll works whether been finished...\n");
        for (unsigned short widx = 0; widx < WORK_NUM; ++widx) {
                struct work_struct *w = w_desc[widx];
                do {
                        if (work_is_finished(w)) {
                                printf("main thread: work -- %p has been finished,put it\n",
                                       w);
                                work_put(w);
                                break;
                        }
                } while (1);
        }
        
        printf("main thread: stop workers\n");
        stop_all_workers(wl);

        printf("main thread: put work list\n");
        work_list_put(wl);

        printf("main thread: process exit\n");
        
        exit(EXIT_SUCCESS);
}
