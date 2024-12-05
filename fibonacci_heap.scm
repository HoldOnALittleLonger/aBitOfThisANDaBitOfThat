;;  assist procedures

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
      )
  )

;;  fibonacci heap

;;  fibHeap-null-forest - no tree-heads in forest.
(define fibHeap-null-forest '())

;;  fibHeap-forest - declare and initialize the Eva
;;  #  fibHeap-forest = { th0 th1 th2 ... thk}
;;                      th0 : tree-head with rank is 0
;;                      ...
;;                      thk : tree-head with rank is @k
(define fibHeap-forest fibHeap-null-forest)

;;  fibHeap-forest-insert - insert a tree-head into the forest
;;  @tree-head:             the tree-head to insert
;;  @forest:                the forest to be inserted
;;  return:                 new forest
(define (fibHeap-forest-insert tree-head forest)
  (cons tree-head forest)
  )

;;  fibHeap-makeup-single-tree-forest - make up forest it just contains
;;                                      a tree
;;  @tree-head:                         the tree-head contains the
;;                                      tree
;;  return:                             the single tree forest
;;  #  this procedure is used by (fibHeap-insert) as usual.
(define (fibHeap-makeup-single-tree-forest tree-head)
  (cons tree-head fibHeap-null-forest)
  )

;;  fibHeap-forest-tree-head - retrieve the first tree-head in the forest
;;  @forest:                   the forest
;;  return:                    nil => @forest is null
;;                             1st tree-head
(define (fibHeap-forest-tree-head forest)
  (if (null? forest)
      '()
      (car forest)
      )
  )

;;  call (car) or (cdr) on '() is an error

;;  fibHeap-forest-rest - the rest of the forest
;;  @forest:              the forest
;;  return:               nil => @forest is null
;;                        (cdr @forest)
(define (fibHeap-forest-rest forest)
  (if (null? forest)
      '()
      (cdr forest)
      )
  )

;;  heap-order property : key(parent) > key(child)
;;  only the two trees have same rank is allowed to merge.
;;  =  (rank t1) (rank t2) =>
;;  (merge t1 t2)
;;    =>  if (> (key (root t1)) (key (root t2)))
;;           (merge t2 t1)
;;           (become-child t2 t1)
;;             =>  link t2 into t1,t2 as a child of t1 now
;;    =>  update t1's rank
;;        (set! (rank t1) (+ 1 (rank t1)))  

;;  fibHeap-makeup-tree-head - each tree-head is an element in the forest,
;;                             tree-head { rank trees }
;;  @rank:                     the rank
;;  @trees:                    list of trees each has the same rank k
;;  #  tree-head is not a list,it is a pair combined 
;;     @rank and @trees
;;     call to (car) or (cdr) to retrieve the element in a pair
;;     is legal except @pari is '()
(define (fibHeap-makeup-tree-head rank . trees)
  (cons rank trees)
  )

;;  fibHeap-tree-head-placeholder - no NIL is possible to appears in list
;;                                  as (cdr) that the position is not
;;                                  end of list.
;;  #  @trees of tree-head-placeholder is NIL
(define (fibHeap-tree-head-placeholder rank)
  (fibHeap-makeup-tree-head rank)
  )

(define (fibHeap-tree-head-is-placeholder? tree-head)
  (null? (fibHeap-tree-head-trees tree-head))
  )

(define (fibHeap-tree-head-rank tree-head) (car tree-head))
(define (fibHeap-tree-head-trees tree-head) (cdr tree-head))

;;  fibHeap-tree-head-extend-trees - link new trees into the tree-head
;;  @tree-head:                      the tree head to link
;;  @new-trees:                      list of new trees
;;  return:                          new list that trees have been linked
(define (fibHeap-tree-head-extend-trees tree-head new-trees)
  (append (fibHeap-tree-head-trees tree-head) new-trees)
  )

;;  fibHeap-tree-head-combine - combine two tree-heads
;;  @tree-head1:                1st tree-head
;;  @tree-head2:                2nd tree-head
;;  return:                     a combined tree-head
;;                              '() => these two tree-heads have different rank
;;  #  combine is not "merge",this procedure does not merge
;;     the two tree-heads,just places the two tree-lists in a
;;     box.
(define (fibHeap-tree-head-combine tree-head1 tree-head2)
  (cond
   ((fibHeap-tree-head-is-placeholder? tree-head1)
    tree-head2)
   ((fibHeap-tree-head-is-placeholder? tree-head2)
    tree-head1)
   (else
    (if (= (fibHeap-tree-head-rank tree-head1)
           (fibHeap-tree-head-rank tree-head2)
           )
        (fibHeap-tree-head-extend-trees tree-head1
                                        (fibHeap-tree-head-trees tree-head2)
                                        )
        '()
        )
    )
   )
  )

(define (fibHeap-tree-head-modify-rank tree-head new-rank)
  (fibHeap-makeup-tree-head new-rank (fibHeap-tree-head-trees tree-head))
  )

;;  fibHeap-node-lc-counter-max -
;;    a node is not root,then everytime it lose a child,
;;    node.lc-counter must be increased.
;;    the maximum value of the node.lc-counter is
;;    defined by @fibHeap-node-lc-counter-max,it equals to
;;    2.when node.lc-counter reach the max,have to cut it away
;;    to its parent(cascade cutting).
(define fibHeap-node-lc-counter-max 2)

;;  fibHeap-node-lc-counter-init - initial value is zero.
(define fibHeap-node-lc-counter-init 0)

;;  fibHeap-makeup-node - the node in the fibHeap-tree-head.
;;  @key:                 key value
;;  @childs:              list of childs
;;  @lc-counter:          count how many childs this node had been lost
;;  @r-bit:               a bit that indicates whether the node as
;;                        a root node
(define (fibHeap-makeup-node key childs lc-counter r-bit)
  (list key childs lc-counter r-bit)
  )

(define (fibHeap-makeup-init-node key)
  (fibHeap-makeup-node key '()
                       fibHeap-node-lc-counter-init
                       fibHeap-node-r-bit-off
                       )
  )

(define fibHeap-node-r-bit-on 1)
(define fibHeap-node-r-bit-off 0)

(define (fibHeap-node-key node) (car node))
(define (fibHeap-node-childs node) (cadr node))
(define (fibHeap-node-lc-counter node) (caddr node))
(define (fibHeap-node-r-bit node) (cadddr node))

;;  fibHeap-node-decrease-key - node scoped DecreaseKey,
;;                              do not mark its parent
;;  @node:                      the node to decrease key
;;  @delta:                     delta value
;;  return:                     new node that had decreased
(define (fibHeap-node-decrease-key node delta)
  (fibHeap-makeup-node
   (- (fibHeap-node-key node)
      delta)
   (fibHeap-node-childs node)
   (fibHeap-node-lc-counter node)
   (fibHeap-node-r-bit node)
   )
  )


(define (fibHeap-node-modify-childs node new-childs)
  (fibHeap-makeup-node
   (fibHeap-node-key node)
   new-childs
   (fibHeap-node-lc-counter node)
   (fibHeap-node-r-bit node)
   )
  )

;;  fibHeap-node-increase-lc-counter - increase lc-counter of node
;;  @node:                             the node to increase lc-counter
;;  return:                            new node that lc-counter had 
;;                                     been increased
(define (fibHeap-node-increase-lc-counter node)
  (fibHeap-makeup-node
   (fibHeap-node-key node)
   (fibHeap-node-childs node)
   (+ (fibHeap-node-lc-counter node)
      1)
   (fibHeap-node-r-bit node)
   )
  )

;;  fibHeap-node-modify-r-bit - modify nodes' r-bit
;;  @node:                      the node to modify
;;  @state:                     the new state of r-bit
;;  return:                     new node that r-bit had been
;;                              set up
(define (fibHeap-node-modify-r-bit node state)
  (fibHeap-makeup-node
   (fibHeap-node-key node)
   (fibHeap-node-childs node)
   (fibHeap-node-lc-counter node)
   state
   )
  )

(define (fibHeap-node-set-r-bit node)
  (fibHeap-node-modify-r-bit node fibHeap-node-r-bit-on)
  )
(define (fibHeap-node-clear-r-bit node)
  (fibHeap-node-modify-r-bit node fibHeap-node-r-bit-off)
  )
   
;;  fibHeap-is-node-haveto-be-cut? - is the node should to be
;;                                   cut off?
;;  #  lc-counter = 2 AND !r-bit
;;     generally,when a node becomes root,its lc-counter
;;     would be reset to zero.                                 
(define (fibHeap-is-node-haveto-be-cut? node)
  (and (= (fibHeap-node-lc-counter node)
          fibHeap-node-lc-counter-max)
       (not (fibHeap-node-r-bit node))
       )
  )

;;  fibHeap-insert - fibonacci heap operation INSERT
;;  @key:            insert a new key into the @forest
;;  @forest:         the forest to insert
;;  return:          new forest that @key had been inserted
(define (fibHeap-insert key forest)
  (fibHeap-merge
   (fibHeap-makeup-single-tree-forest
    (fibHeap-makeup-tree-head 0
                              (fibHeap-makeup-init-node key)
                              )
    )
   forest)
  )

;;  fibHeap-merge - fibonacci heap operation MERGE
;;  @forest1:       the first forest
;;  @forest2:       the second forest
;;  return:         forest it had been merged
;;  #  this procedure does lazy-merge,that is
;;       only combine the tree-heads have the
;;       same rank instead to merge them really
(define (fibHeap-merge forest1 forest2)
  (define (fibHeap-lazy-merge f1 f2)
    (define (traverse-all-and-combine f1 f2)
      (let
          ((tree-head1 (fibHeap-forest-tree-head f1))
           (tree-head2 (fibHeap-forest-tree-head f2))
           )
        (fibHeap-forest-insert
         (fibHeap-tree-head-combine tree-head1 tree-head2)
         (fibHeap-lazy-merge
          (fibHeap-forest-rest f1)
          (fibHeap-forest-rest f2)
          )
         )
        )
      )

    (cond
     ((null? f1)
      f2)
     ((null? f2)
      f1)
     (else
      (traverse-all-and-combine f1 f2)
      )
     )
    )

  (fibHeap-lazy-merge f1 f2)
  )

;;  we can not record the positions of nodes,because 
;;  of it is that all nodes are linked into a layered
;;  list,no address type such @pointer in C/C++ is
;;  available be there.
;;  maybe there has a way that allows we to modify
;;  the environment bindings(interpreter feature)
;;  but such way will put all position records in
;;  single environment.
(define (fibHeap-findout-a-node key forest))

;;  fibHeap-DecreaseKey - decrease the key of @node
;;                        in @delta
;;  @node:                the node contains the key
;;                        to be decreased
;;  @delta:               decrease value
;;  return:               new node that the key
;;                        had been decreased
;;  #  DecreaseKey may cause cutting off or
;;     cascade cutting,maybe several times.
;;
;;     node @v
;;     DecreaseKey to @v
;;     < (key @v) (key (parent @v))
;;       =>  cut off @v
;;           @v becomes root of a new tree would
;;           be inserted to forest,the lc-counter
;;           becomes zero,r-bit on
;;           mark (parent @v)
;;           =>
;;             = (lc-counter (parent @v))
;;               lc-counter-max
;;             =>
;;               cascade cutting to (parent @v)
(define (fibHeap-DecreaseKey node delta))

;;  fibHeap-DeleteMin - delete the node contains a minimum
;;                      key in the forest
;;  @forest:            DeleteMin operate on
;;  return:             forest after DeleteMin done
;;  #  DeleteMin cause merges to all tree-heads they just
;;     added into forest by former Merges but
;;     have not been merged.
(define (fibHeap-DeleteMin forest))
           
           
                                    
      
