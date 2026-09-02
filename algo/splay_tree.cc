/**
 * Simple splay tree demo,no rebalance.
 */
#include <system_error>
#include <exception>
#include <iostream>

#include <cstddef>
#include <cassert>

template<typename _Tp>
class Splay_tree {
public:
  struct tree_node {
    _Tp item;
    tree_node *left;
    tree_node *right;

    tree_node(_Tp x)
      : item(x)
    {
      left = right = NULL;
    }
  };

  Splay_tree()
    : splay_tree_(NULL)
  { }

  Splay_tree(_Tp x)
  {
    tree_node *root = NULL;
    try {
      root = new tree_node(x);
    } catch (std::bad_alloc&) {
      std::runtime_error("allocation failed.");
    }
    splay_tree_ = root;
  }

  void insert(const _Tp& key)
  {
    auto __insert_recursion = [key](this auto& __self, tree_node *root)
      -> tree_node *
    {
      if (!root) {
        tree_node *new_node = NULL;
        try {
          new_node = new tree_node(key);
        } catch (std::bad_alloc&) {
          std::runtime_error("allocation failed.");
        }
        return new_node;
      }

      /**
       * We execute rotations on the return path.
       * The first time we get arrive to the proper position
       * where we can insert new node.
       * On the return path,we move the new node to the root
       * through multiple times of rotation.
       */
      if (root->item > key) {
        root->left = __self(root->left);
        return __right_rotate(root);
      }
      else if (root->item < key) {
        root->right = __self(root->right);
        return __left_rotate(root);
      }
      else
        return root;
    };

    splay_tree_ = __insert_recursion(splay_tree_);
  }  

  /* just a variant of INSERTION */
  tree_node *find(const _Tp& key)
  {
    auto __find_recursion = [key](this auto& __self, tree_node *root)
      -> tree_node *
    {
      if (!root)
        return root;

      if (root->item > key) {
        root->left = __self(root->left);
        return __right_rotate(root);
      }
      else if (root->item < key) {
        root->right = __self(root->right);
        return __left_rotate(root);
      }
      else
        return root;
    };

    splay_tree_ = __find_recursion(splay_tree_);
    return splay_tree_;
  }
  
  const tree_node *get_root_const(void) const
  {
    return splay_tree_;
  }
  
private:
  /* rotate direction: from right to left */
  static tree_node *__left_rotate(tree_node *root)
  {
    if (!root)
      return root;
    
    auto right_child = root->right;
    if (!right_child)
      return root;

    auto rl_child = right_child->left;
    root->right = rl_child;
    right_child->left = root;
    return right_child;
  }

  /* rotate direction: from left to right */
  static tree_node *__right_rotate(tree_node *root)
  {
    if (!root)
      return root;
    
    auto left_child = root->left;
    if (!left_child)
      return root;

    auto lr_child = left_child->right;
    root->left = lr_child;
    left_child->right = root;
    return left_child;
  }

  tree_node *splay_tree_;
};

int main(void)
{
  Splay_tree<int> stree;

  for (unsigned int i(0); i < 10; ++i) {
    stree.insert(i);
    std::cout << "root->value is " << stree.get_root_const()->item << std::endl;
    assert(stree.get_root_const()->item == i);
  }

  for (unsigned int i(10); i < 20; ++i) {
    stree.insert(i);
    std::cout << "root->value is " << stree.get_root_const()->item << std::endl;
    assert(stree.get_root_const()->item == i);
  }

  stree.find(11);
  assert(stree.get_root_const()->item == 11);
  
  return 0;
}

