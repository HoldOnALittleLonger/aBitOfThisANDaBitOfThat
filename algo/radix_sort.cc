#include <cstddef>
#include <cstring>
#include <list>

/**
 * Radix Sort :
 *   suppose radix is 10.
 *   input has range [0 -- xxxx].
 *           x x x x x x x
 *                       1
 *                     10
 *                   100
 *                 1000
 *               10000
 *             100000
 *           1000000
 *  for each element from @input
 *    we first sort them on 'g" of "abcdefg"
 *    next,we sort them on 'f' of "abcdefg"
 *    then it is 'e' of "abcdefg"
 *    and so on.
 *    until we reached the maximum range.
 *  because there may have some elements have the
 *  same radix_th number,we need to put these elements
 *  that should be contained in the same bucket to a
 *  table.
 */

template<typename _Tp>
int radix_sort(_Tp input[], std::size_t n,
               std::size_t max_valid, std::size_t radix,
               std::list<_Tp> bucket[], std::size_t m)
{
  // we suppose radix is 10.

  if (m < 11)
    return -1;

  std::size_t the_radix(radix);
  
  while (the_radix < max_valid) {
    std::size_t extractor = the_radix / radix;

    for (unsigned int i(0); i < n; ++i) {
      std::size_t subscript = (input[i] % the_radix) / extractor;
      bucket[subscript].push_back(input[i]);
    }

    memset(input, 0, n);
    std::size_t index_of_input(0);
    for (unsigned int i(0); i < m; ++i) {
      // put back to input array.

      std::list<_Tp> &table(bucket[i]);
      for (auto e : table)
        input[index_of_input++] = e;

      table.clear();
    }

    the_radix *= radix;
  }

  return 0;
}
