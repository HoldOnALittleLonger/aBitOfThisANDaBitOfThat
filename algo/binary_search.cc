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
  if (!size)
    return NULL;

  std::size_t middle(size / 2);

  if (array[middle] == x)
    return &array[middle];
  else if (array[middle] < x)
    return binary_search(x, array + middle + 1, size - middle - 1);
  else                      // ^skip the tested element
    return binary_search(x, array, middle);
}
