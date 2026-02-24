#include <cstddef>

/**
 * GCD algorithm :
 *   Compute the maximum common factor of @a and @b.
 */

static inline
template<typename _Tp>
_Tp get_remainder(_Tp a, _Tp b)
{
  return a - (a / b) * b;
}

template<typename _Tp>
_Tp gcd(_Tp a, _Tp b)
{
  if (!b)
    return a;

  _Tp remainder(0);
  remainder = get_remainder(a, b);
  return gcd(b, remainder);
}
