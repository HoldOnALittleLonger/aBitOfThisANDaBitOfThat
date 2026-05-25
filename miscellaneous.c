/**
 * File used to records some useful procedures or other
 * something.
 */

#include <stddef.h>

/**
 * count_openbits - count the number of opened bits of
 *                  a given value.
 *                  opened means the bit is set to 1.
 * @v:              value
 * return:          number of opened bits
 * # if CPU supports instruction "popcnt",that is more
 *   faster than this procedure.
 */
unsigned char count_openbits(unsigned int v)
{
        unsigned char count = 0;
        while (v) {
                ++count;
                v &= (v - 1);
        };

        return count;
}

/**
 * Tatham's Coroutine.
 * The 'case' label of switch statement in C can appears
 * everywhere in the switch block.
 * That means it can also appears inside a do-while loop.
 * Because @tc_state is a static variable,the first time
 * enter the function used this coroutine,will returns
 * from the do-while loop.
 * The second time enter the function,because @tc_state
 * is updated at previous calling,the switch statement
 * will let the execution flow jump the label where
 * case is '__LINE__'.
 * If the do-while loop is nested inside an external
 * loop,then start from the end of do-while loop,
 * the rest of that loop will executes.
 * So,this is a simply coroutine implementation in C
 * for single-thread scenario.
 */
#define tatham_coroutine_begin                  \
        static int tc_state;                    \
        switch (tc_state) {                     \
        case 0:

#define tatham_coroutine_return(x)              \
        do {                                    \
                tc_state = __LINE__;            \
                return x;                       \
        case __LINE__:;                         \
        } while (0)

#define tatham_coroutine_finish                 \
        }

int use_tatham_coroutine(void)
{
        static int iter;
        
        tatham_coroutine_begin;

        for (iter = 0; iter < 100; ++iter)
                tatham_coroutine_return(iter);

        tatham_coroutine_finish;
}
/*        ||
 *        VV
 */
//int use_tatham_coroutine(void)
//{
//        static int iter;
//
//        /* tatham_coroutine_begin */
//        static int tc_state;
//        switch (tc_state) {
//        case 0:;
//
//                /* prepare await */
//                for (iter = 0; iter < 100; ++iter)
//                        do {
//                                tc_state = __LINE__;
//                                return iter;
//        case __LINE__:;
//                        } while (0);
//
//        /* tatham_coroutine_finish */
//        }
//}

#ifdef MISC_EXAMPLE

#include <unistd.h>
#include <stdio.h>

void example_count_openbits(void)
{
        unsigned int value = 0b11010011;
        printf("opened bits of value %u is %hu\n",
               value, count_openbits(value));
}

void example_tatham_coroutine(void)
{
        for (unsigned i = 0; i < 8; ++i) {
                printf("loop times - %u\n", i);
                printf("tatham_coroutine returned - %d\n",
                       use_tatham_coroutine());
                sleep(1);
        }

        printf("%s end.\n", __FILE__);
}

#endif /* MISC_EXAMPLE */
