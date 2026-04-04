/**
 * shell sort : (diminishing increment sort)
 *   increment sequence {Ht} : (H1 := 1, t := 1, 2, 3, ...)
 *   after sort with Hk,any index has order Ai <= Ai + Hk.
 *   if we use j := i + Hk,then Ai and Aj must been ordered.
 *   at this time,the input is Hk-sorted.
 *   if input is Hk-sorted,even we do Hk-1 sorting,Hk-2 sorting, ... ,
 *   the input still is Hk-sorted.
 *
 *   Hk-sort :
 *     for each element in range
 *     [Hk, Hk + 1, Hk + 2, ..., N - 1]
 *     we place the elements in the range
 *     [index, index - Hk, index - 2 * Hk, index - 3 * Hk, ..., Hk]
 *     ! Hk-sort actually is do insertion sort on Hk independent
 *       subsequences.
 *     ! if we pick Hk through "N / 2",Hk finally will become 1.
 */

#include <cstddef>

template<typename _Tp>
void shell_sort(_Tp input[], size_t n)
{
  size_t inc(n / 2);

  while (inc > 0) {
    
    /* [Hk, N - 1] */
    for (unsigned int i(inc); i < n - 1; ++i) {
      _Tp tmp = input[i];
      unsigned int j = 0;

      /* insertion sort */
      for (j = i; j >= inc; j -= inc)
        if (tmp < input[j - inc])
          input[j] = input[j - inc];
        else
          break;

      input[j] = tmp;
    }

    inc /= 2;
  }

}
