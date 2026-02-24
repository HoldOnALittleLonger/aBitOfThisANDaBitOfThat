#include <cstddef>

/**
 * binary search algorithm :
 *   @x => the element we want to find
 *   @array => array of elements
 *   return position of @x in @array
 *   
 *   Requirement : @array must been sorted
 *   
 *   Time complexity : O(logN)
 */

template<typename _Tp>
const _Tp *binary_search(const _Tp& x, const _Tp array[], std::size_t size)
{
  std::size_t middle(size / 2);
  if (!middle)
    return NULL;

  const auto left_side(array);
  const auto right_side(array + middle);

  if (left_side[middle - 1] == x)
    return &left_side[middle - 1];
  else if (left_size[middle - 1] < x)
    return binary_search(x, right_side, size - middle);
  else
    return binary_search(x, left_side, middle);
}
