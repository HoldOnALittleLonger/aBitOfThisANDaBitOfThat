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
 *     while @iL < iR
 *     do
 *       while @S[++@iL] < @v ;
 *       while @S[--@iR] > @v ;
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

#define CUTOFF (3)
void quick_sort_helper(int input[], size_t left, size_t right)
{
        if (left + CUTOFF >= right) {
                insertion_sort(input + left, right - left + 1); 
        } else {
                int iL = left;
                int iR = right;
                int iV = (left + right) / 2;
                int tmp = 0;

                /* swap [0], [middle], [last] */
                if (input[iV] > input[iR]) {
                        tmp = input[iR];
                        input[iR] = input[iV];
                        input[iV] = tmp;
                }
                if (input[iL] > input[iR]) {
                        tmp = input[iR];
                        input[iR] = input[iL];
                        input[iL] = tmp;
                }
                if (input[iL] > input[iV]) {
                        tmp = input[iV];
                        input[iV] = input[iL];
                        input[iL] = tmp;
                }                

                /* get @v out sequence */
                int v = input[iV];
                tmp = input[--iR];
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

                /* get @v back sequence */
                tmp = input[iL];
                input[iL] = v;
                input[right - 1] = tmp;

                /* remaining */
                quick_sort_helper(input, left, i - 1);
                quick_sort_helper(input, i + 1, right);
        }
}
