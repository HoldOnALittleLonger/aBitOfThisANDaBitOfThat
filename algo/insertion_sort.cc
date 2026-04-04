/**
 * insertion sort :
 *   @input: input data set
 *   @n:     data length
 *   @p:     position
 *   
 *   for @p := 1 to @p := @n - 1,the element in the position range [0, @p]
 *   are ordered.        #^the last element in array
 *
 *   suppose S1 { ... } is ordered,S2 { ... } is the rest input.
 *   for @p in S2
 *     remove @input[@p] from S2,and put it into S1 to order it
 *   the initial S1 only contains @input[0].
 *   we traverse S1 from the end to the head.
 *
 *   [ S1 0 | S1 1 | S1 2 | ... | @p - 1 | @p | @p + 1 | @p + 2 | ... | @n - 1]
 *   |<---------------- S1 ------------->|<------------- S2 ----------------->|
 *                                ^S1 end  ^S2 head
 *   |<----------- test range --------------->|
 *                 # test and move other elements
 */

#include <cstddef>

template<typename _Tp>
void insertion_sort(_Tp input[], size_t n)
{
  if (n <= 1)
    return;

  /* S2 */
  for (unsigned int p(1), p < n - 1; ++p) {
    _Tp tmp = input[p];
    unsigned int i(p);

    /* S1 */
    while (i > 0) {
      if (input[i - 1] > tmp)
        input[i] = input[i - 1]; // move elements to gain position for @input[@p]

      --i;
    }
    input[i] = tmp; // the right position for @input[@p],because other
                    // elements have been moved,thus a hole will produce.
  }

}
