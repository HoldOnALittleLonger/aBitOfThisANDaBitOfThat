type TreeNode<T> = Box::<tree_node_struct<T>>;

struct tree_node_struct<T> {
    item: T,
    left: Option<TreeNode<T>>,
    right: Option<TreeNode<T>>,
}

struct splay_tree<T> {
    root: Option<TreeNode<T>>,
}

impl<T: std::cmp::PartialOrd> splay_tree<T> {
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
    let stree: splay_tree<i32> = splay_tree {
        root: None,
    };
}
