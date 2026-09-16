/* type-definition for tree node */
type TreeNode<T> = Box::<tree_node_struct<T>>;

/*
 * tree_node_struct<> - template for tree node
 * @item:               the value of this node
 * @left:               left child tree
 * @right:              right child tree
 */
#[derive(Debug)]
struct tree_node_struct<T> {
    item: T,
    left: Option<TreeNode<T>>,
    right: Option<TreeNode<T>>,
}

impl<T> tree_node_struct<T> {
    fn new(v: T) -> Self {
        Self {
            item: v,
            left: None,
            right: None,
        }
    }
}

/*
 * splay_tree<> - dummy head of splay tree
 */
#[derive(Debug)]
struct splay_tree<T> {
    root: Option<TreeNode<T>>,
}

impl<T: std::cmp::PartialOrd> splay_tree<T> {
    /*
     * __insert_recursion - insert the given value through tree recursion
     * @root:               the root node to test
     * @v:                  the value
     * return:              wrapped new root,been rotated
     */
    fn __insert_recursion(root: Option<TreeNode<T>>, v: T)
                          -> Option<TreeNode<T>> {
        let Some(mut troot) = root else {
            /* current root is NULL */
            return Some(TreeNode::<T>::new(tree_node_struct::<T>::new(v)));
        };

        /*
         * we exeucte rotations on the return path.
         */
        if troot.item > v {
            /* to left */
            let troot_left = troot.left.take();
            troot.left = Self::__insert_recursion(troot_left, v);
            return Some(Self::__right_rotate(troot));
        } else if troot.item < v {
            /* to right */
            let troot_right = troot.right.take();
            troot.right = Self::__insert_recursion(troot_right, v);
            return Some(Self::__left_rotate(troot));
        } else {
            /* been in tree */
            Some(troot)
        }
    }

    /*
     * insert - public method to start insertion
     * @self:   mutable reference to self
     * @v:      value to insert
     */
    fn insert(&mut self, v: T) {
        self.root = Self::__insert_recursion(self.root.take(), v);
    }
    
    fn __left_rotate(mut root: TreeNode<T>) -> TreeNode<T> {
        let Some(mut rchild) = root.right.take() else {
            return root;
        };

        let Some(rlchild) = rchild.left.take() else {
            rchild.left = Some(root);
            return rchild;
        };

        root.right = Some(rlchild);
        rchild.left = Some(root);
        rchild
    }

    fn __right_rotate(mut root: TreeNode<T>) -> TreeNode<T> {
        let Some(mut lchild) = root.left.take() else {
            return root;
        };

        let Some(lrchild) = lchild.right.take() else {
            lchild.right = Some(root);
            return lchild;
        };

        root.left = Some(lrchild);
        lchild.right = Some(root);
        lchild
    }
}

fn main() {
    let mut stree: splay_tree<i32> = splay_tree {
        root: None,
    };

    stree.insert(0);
    stree.insert(1);
    stree.insert(2);
    stree.insert(9);
    stree.insert(6);
    stree.insert(12);
    stree.insert(23);
    stree.insert(-19);
    dbg!(stree);
}
