#include <exception>
#include <memory>
#include <expected>
#include <print>

#include <cstdint>
#include <cstddef>

template<typename _Tp>
struct binary_heap_struct {
  
  explicit binary_heap_struct(std::size_t init_size)
  {
    capacity_ = !init_size ? DEFAULT_CAPACITY : init_size;
    try {
      container_.reset(new _Tp[capacity_]);
    } catch (std::bad_alloc&) {
      std::runtime_error("error: allocate memory failed.");
    }
    valid_len_ = 0;
  }

  bool insert(const _Tp& new_value);
  std::expected<_Tp, int> delete_min(void);
  
  bool decrease_key(std::size_t idx, const _Tp& delta)
  {
    if (!idx || idx > this->__length())
      return false;
    container_[idx] -= delta;
    __percolate_up(idx);
    return true;
  }
  
  bool increase_key(std::size_t idx, const _Tp& delta)
  {
    if (!idx || idx > this->__length())
      return false;
    container_[idx] += delta;
    __percolate_down(idx);
    return true;
  }
  
private:
  enum {
    DEFAULT_CAPACITY = 32,
  };

  std::size_t __capacity(void)
  {
    return capacity_;
  }

  std::size_t __length(void)
  {
    return valid_len_;
  }

  void __inc_length(void)
  {
    ++valid_len_;
  }

  void __dec_length(void)
  {
    ++valid_len_;
  }

  std::size_t __left_idx(std::size_t idx)
  {
    return idx * 2;
  }

  std::size_t __right_idx(std::size_t idx)
  {
    return __left_idx(idx) + 1;
  }

  std::size_t __p_idx(std::size_t idx)
  {
    return idx / 2;
  }

  void __percolate_up(std::size_t idx);
  void __percolate_down(std::size_t idx);

  void __swap_elements(std::size_t lhs, std::size_t rhs)
  {
    auto tmp = container_[lhs];
    container_[lhs] = container_[rhs];
    container_[rhs] = tmp;
  }

  /* _Tp[] => we will allocate array */
  std::unique_ptr<_Tp[]> container_;
  std::size_t capacity_;
  std::size_t valid_len_;
};

template<typename _Tp>
void binary_heap_struct<_Tp>::__percolate_up(std::size_t idx)
{
  if (idx > this->__length())
    std::runtime_error("error: BUG: bad idx to __percolate_up().");

  for (std::size_t pidx(0); idx > 0; idx = pidx) {
    pidx = __p_idx(idx);
    if (!pidx)
      break;

    if (container_[pidx] > container_[idx])
      __swap_elements(pidx, idx);
    else
      break;
  }
}

template<typename _Tp>
void binary_heap_struct<_Tp>::__percolate_down(std::size_t idx)
{
  if (idx > this->__length())
    std::runtime_error("error: BUG: bad idx to __percolate_down().");

  for (std::size_t next_idx(0); idx < this->__length(); idx = next_idx) {
    auto lidx = __left_idx(idx);
    auto ridx = __right_idx(idx);

    if (lidx < this->__length() && ridx < this->__length())
      next_idx = container_[lidx] > container_[ridx] ? ridx : lidx;
    else if (lidx < this->_length())
      next_idx = lidx;
    else
      break;

    if (container_[idx] > container_[next_idx])
      __swap_elements(idx, next_idx);
    else
      break;
  }
}

template<typename _Tp>
bool binary_heap_struct<_Tp>::insert(const _Tp& new_value)
{
  /**
   * __length() returns number of valid elements.because [0]
   * is not counted in the valid elements,thus its result will
   * be the index of last element in @container_,and the next
   * void hole is the result of idx + 1 .
   */
  std::size_t vidx = this->__length() + 1;
  if (vidx >= this->__capacity())
    return false;

  container_[vidx] = new_value;
  this->__percolate_up(vidx);
  this->__inc_length();
  return true;
}

template<typename _Tp>
std::expected<_Tp, int> binary_heap_struct<_Tp>::delete_min(void)
{
  if (!this->__length()) {
    return std::unexpected<int>(0);
  }

  std::expected<_Tp, int> result(container_[1]);
  
  auto last_idx = this->__length();
  std::size_t idx = 1;
  for (auto next_idx(0); idx < this->__length(); idx = next_idx) {
    auto lidx = __left_idx(idx);
    auto ridx = __right_idx(idx);

    if (lidx < this->__length() && ridx < this->__length())
      next_idx = container_[lidx] > container_[ridx] ? ridx : lidx;
    else if (lidx < this->__length())
      next_idx = lidx;
    else
      break;

    /**
     * We need to take care of void hole that appears because
     * the root element is removed from the heap.
     * We  use the last element in the array to fill it.
     * So,we percolate up the elements on the path,when the
     * loop stopped,@idx points to the appropriate position.
     */
    if (container_[last_idx] > container_[next_idx])
      container_[idx] = container_[next_idx];
    else
      break;
  }

  __swap_elements(idx, last_idx);
  this->__dec_length();

  return result;
}

int main(void)
{
  binary_heap_struct<int> bh(16);
  const int inputs[] = {
    2, 3, 32, 6, 8, 22
  };

  for (unsigned int i = 0; i < sizeof(inputs) / sizeof(int); ++i)
    if (!bh.insert(inputs[i]))
      std::runtime_error("error: insertion failed.");

  /* auto => std::expected<> */
  auto min = bh.delete_min();
  if (min.has_value())
    std::println("min is {}", min.value());
  else
    std::println("delete_min failed.");

  return 0;
}




