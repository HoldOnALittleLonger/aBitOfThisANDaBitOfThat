#include <cstddef>

/**
 * Bucket Sort :
 *   array[n] => [ ] [ ] [ ] [ ] [ ] ... [ ]
 *               |<------- m buckets ----->|
 *   we traverse @array,for each element,place it into
 *   the bucket has number array[i].
 *   this method can only work for unsigned type.
 *   for the input contains negative value,we should
 *   prepare another bucket-set for store the negative values.
 */

template<typename _Tp>
int bucket_sort(const _Tp input[], std::size_t n,
                unsigned int maximum_limit,
                unsigned int minimum_limit,
                _Tp buckets[], std::size_t m,
                _Tp negv_buckets[], std::size_t nm)
{
  /**
   * we expect there is a suitable bucket to store the
   * maximum element or to store the minimum element
   * from input.
   * the reason of " + 1 " is that array is start from
   * subscript 0.
   */
  if (maximum_limit > m + 1 || minimum_limit > nm + 1)
    return -1;

  _Tp tmp(0);
  for (unsigned int i(0); i < n; ++i) {
    tmp = input[i];
    if (tmp < 0)
      negv_buckets[(tmp * -1)] = tmp;
    else
      buckets[tmp] = tmp;
  }
}
