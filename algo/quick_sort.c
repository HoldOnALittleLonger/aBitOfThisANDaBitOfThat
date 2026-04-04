/**
 * quick sorting :
 *   suppose input S is [11, 66, 22, 212, 3, 28, 72, 55, 4]
 *   size is 9.
 *   the first quick sort :
 *     input: @S, left: 0 => S[0], right: 8 => S[8]
 *     index @iV is (left + right) / 2 => 4
 *     @iL : 11
 *     @iV : 3
 *     @iR : 4
 *     sort @iL @iV @iR => 3 .. 4 .. 11
 *     select @v at @iV : 4
 *     swap @v to right - 1 : 3 .. 55 .. 4 11
 *     start sorting :
 *       iL start from 0
 *       iR start from right - 1
 *     while @iL < @iR
 *     do
 *       while @S[++@iL] < @v 
 *       while @S[--@iR] > @v 
 *
 *       # the first time @iL and @iR stopped
 *         @iL : 1 => 66
 *         @iR : 0 => 3
 *         not @iL < @iR,stop loop,and sort left part and right part
 *
 *       if @iL < @iR
 *         swap @S[@iL] and @S[@iR]
 *     done
 *     swap @S[@iL] and @S[right - 1]
 *     # after swap : 3 4 ... 55 ... 66 11
 *     
 *     start the second quick sort :
 *       input: @S, left: @left, right: @iL - 1
 *     start the second quick sort :
 *       input: @S, left: @iL + 1, right: @right
 */

#include <stddef.h>

#define CUTOFF 3

static size_t swap_and_return_vidx(int input[], size_t left, size_t right)
{
        size_t iV = (left + right) / 2;
        int tmp = 0;

        if (input[iV] > input(right)) {
                tmp = input[right];
                input[right] = input[iV];
                input[iV] = tmp;
        }

        if (input[left] > input[right]) {
                tmp = input[right];
                input[right] = input[left];
                input[left] = tmp;
        }

        if (input[left] > input[iV]) {
                tmp = input[left];
                input[left] = input[iV];
                input[iV] = tmp;
        }

        return iV
}

void quick_sort(int input[], size_t left, size_t right)
{
        if (left + CUTOFF >= right) {
                insertion_sort(input + left, right - left + 1); 
        } else {
                size_t iL = left;
                size_t iR = right;

                size_t iV = swap_and_return_vidx(input, iL, iR);
                int v = input[iV];

                /**
                 * get @v out to sequence
                 *                           +-----------+
                 *                           |           |
                 *                           |           V
                 * [ left | left + 1 | ... | v | ... | right - 1 | right ]
                 *                           ^         |
                 *                           |         |
                 *                           +---------+
                 */
                int tmp = input[--iR];
                input[iR] = v;
                input[iV] = tmp;

                /* sorting */
                while (iL < iR) {
                        while (input[++iL] < v);
                        while (input[--iR] > v);
                        if (iL < iR) {
                                tmp = input[iR];
                                input[iR] = input[iL];
                                input[iL] = tmp;
                        }
                }

                /* get @v back to sequence */
                tmp = input[iL];
                input[iL] = v;
                input[right - 1] = tmp;

                /* sort the rest */
                quick_sort(input, left, iL - 1);
                quick_sort(input, iL + 1, right);
        }
}
