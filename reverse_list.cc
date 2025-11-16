#include <iostream>
#include <cstddef>

template<typename _Tp>
struct list_node {
  _Tp *item;
  list_node<_Tp> *next;
};

template<typename _Tp>
using list_head = list_node<_Tp>;

template<typename _Tp>
list_node<_Tp> *reverse_list(list_node<_Tp> *node, list_head<_Tp> &head)
{
  if (!node)
    return nullptr;

  auto ret_node = reverse_list(node->next, head);
  node->next = nullptr;

  if (ret_node)
    ret_node = ret_node->next = node;
  else
    head.next = ret_node = node;

  return ret_node;
}

template<typename _Tp>
void insert_something(list_head<_Tp>& head)
{
  // if @new was failed,it will throw an exception,
  // so we do not test whether the memory allocating
  // was successful.
  // after SIGSEGV has delivered,kernel will kill this
  // task and destroy the process address space.
  head.next = new list_node<_Tp>;
  auto next(head.next);

  for (unsigned int i(0); i < 10; ++i) {
    next->item = new _Tp(i);
    next->next = new list_node<_Tp>{nullptr, nullptr};
    next = next->next;
  }
}

template<typename _Tp>
void print_items(list_head<_Tp>& head)
{
  auto next(head.next);
  for (; next != nullptr; next = next->next) {
    if (next->item)
      std::cout << *(next->item) << " ";
  }
  std::cout << std::endl;
}

template<typename _Tp>
void print_nexts(list_head<_Tp>& head)
{
  auto next(head.next);
  std::cout << "Head : " << static_cast<void *>(&head) << std::endl;
  std::cout << "Next : " << static_cast<void *>(head.next) << std::endl;

  while (next) {
    std::cout << "Current : " << static_cast<void *>(next) << std::endl;
    std::cout << "Next : " << static_cast<void *>(next->next) << std::endl;
    next = next->next;
  }
}

template<typename _Tp>
void destroy_list(list_head<_Tp>& head)
{
  auto next(head.next);
  while (next) {
    delete next->item;
    auto to_next = next->next;
    delete next;
    next = to_next;
  }
  
  head.next = nullptr;
}

int main(void)
{
  list_head<int> head{nullptr, nullptr};
  insert_something(head);
  print_nexts(head);
  print_items(head);
  reverse_list(head.next, head);
  print_nexts(head);
  print_items(head);
  destroy_list(head);
  return 0;
}
