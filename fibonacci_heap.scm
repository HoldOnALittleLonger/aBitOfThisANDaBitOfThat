;;  assist procedures

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
      )
  )

(define (sizeof_list the_list)
  (if (null? the_list)
      0
      (+ 1 (sizeof_list (cdr the_list)))
      )
  )

(define (sequence_sum sequence)
  (if (null? sequence)
      0
      (+ (car sequence) (sequence_sum (cdr sequence)))
      )
  )

(define (log-2 x) (/ (log x) (log 2)))

;;  ceiling is a build-in procedure
(define (ceiling-log-2 x) (ceiling (log-2 x)))


;;  fibonacci heap

;;  FOREST

;;  fibHeap-null-forest - no tree-heads in forest
(define fibHeap-null-forest '())

;;  fibHeap-forest - declare and initialize the Eva
;; 
;;  #  fibHeap-forest = { th0 th1 th2 ... thk}
;;                      th0 : tree-head with rank is 0
;;                      ...
;;                      thk : tree-head with rank is @k
(define fibHeap-forest fibHeap-null-forest)

;;  fibHeap-forest-insert - insert a tree-head into the forest
;;  @tree-head:             the tree-head to insert
;;  @forest:                the forest to be inserted
;;  return:                 new forest
;;
;;  #  insert at ahead
(define (fibHeap-forest-insert tree-head forest)
  (cons tree-head forest)
  )

;;  fibHeap-forest-rank - get rank of forest
;;  @forest:              the forest
;;  return:               -1 => null forest
;;                        zero OR positive number => rank
(define (fibHeap-forest-rank forest)
  (if (eq? fibHeap-null-forest forest)
      -1
      (+ 1 (sizeof_list forest))  ;;  start at zero
      )
  )

;;  fibHeap-forest-extend - extends forest
;;  @new-rank:              the new rank for forest
;;  @forest:                forest to extend
;;  return:                 extended forest
;;
;;  #  if forest is null
;;     then construct a new forest via 
;;     (fibHeap-makeup-forest-notrees-with-max-rank)
;;     else if @new-rank <= current rank
;;     then do not extend
;;     else
;;     then fill forest with (new rank - old rank) 
;;     tree-head-placeholder's
(define (fibHeap-forest-extend new-rank forest)
  (define (do-extend new-rank forest)
    (define (product-new-end start-rank counter)
      (if (= counter 0)
          fibHeap-null-forest
          (fibHeap-forest-insert
           (fibHeap-tree-head-placeholder start-rank)
           (product-new-end (+ 1 start-rank) (- counter 1))
           )
          )
      )
    (append forest (product-new-end
                    (+ 1 (fibHeap-forest-rank forest))
                    (- new-rank (fibHeap-forest-rank forest))
                    )
            )
    )
   
  (cond
   ((eq? forest fibHeap-null-forest)
    (fibHeap-makeup-forest-notrees-with-max-rank new-rank)
    )
   ((< new-rank (fibHeap-forest-rank forest))
    forest)
   (else
    (do-extend new-rank forest)
    )
   )
  )



;;  fibHeap-makeup-forest-notrees-with-max-rank - pre-allloc a forest which has a
;;                                                specified maximum rank
;;  @rank:                                        the maximum rank
;;  return:                                       a forest constructed through (@rank + 1)
;;                                                tree-head-placeholders
(define (fibHeap-makeup-forest-notrees-with-max-rank rank)
  (define (makeup rank counter)
    (if (= 0 counter)
        (fibHeap-forest-insert
         (fibHeap-tree-head-placeholder rank)
         fibHeap-null-forest)
        (cons (fibHeap-tree-head-placeholder rank)
              (makeup (+ 1 rank) (- counter 1))
              )
        )
    )

  (makeup 0 (+ 1 rank))
  )

;;  fibHeap-forest-tree-head - retrieve the first tree-head in the forest
;;  @forest:                   the forest
;;  return:                    NIL => null forest
;;                             the 1st tree-head in @forest
(define (fibHeap-forest-tree-head forest)
  (if (null? forest)
      '()
      (car forest)
      )
  )

;;  fibHeap-forest-rest - the rest of the forest
;;  @forest:              the forest
;;  return:               nil => @forest is null
;;                        (cdr @forest)
(define (fibHeap-forest-rest forest)
  (if (null? forest)
      fibHeap-null-forest
      (cdr forest)
      )
  )

;;  fibHeap-forest-place-th - place the tree-head on a correct
;;                            position in forest where have same
;;                            rank as tree-head's
;;  @tree-head:               the tree-head to place
;;  @forest:                  the forest
;;  return:                   NIL => null forest given
;;                            forest had been re-constructed
;;
;;  #  maximum rank of forest >= @tree-head's
;;       it is easy to place
;;     maximum rank of forest < @tree-head's
;;       have to extend forest at first,then
;;       place @tree-head on correct position
(define (fibHeap-forest-place-th tree-head forest)
  (define (place th f)
    (if (= (fibHeap-tree-head-rank th)
           (fibHeap-tree-head-rank (fibHeap-forest-tree-head f)
                                   )
           )
        (fibHeap-forest-insert (fibHeap-tree-head-combine th
                                                          (fibHeap-forest-tree-head f)
                                                          )
                               (fibHeap-forest-rest f)
                               )
        (fibHeap-forest-insert (fibHeap-forest-tree-head f)
                               (place th 
                                      (fibHeap-forest-rest f)
                                      )
                               )
        )
    )

  (cond
   ((eq? fibHeap-null-forest forest)
    forest)
   ((> (fibHeap-tree-head-rank tree-head)
       (fibHeap-forest-rank forest)
       )
    (place tree-head 
           (fibHeap-forest-extend
            (fibHeap-tree-head-rank tree-head)
            forest)
           )
    )
   (else
    (place tree-head forest)
    )
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


;;  TREE-HEAD


;;  fibHeap-makeup-tree-head - each tree-head is an element in the forest,
;;                             tree-head { rank trees }
;;  @rank:                     the rank
;;  @trees:                    list of trees each has the same rank k
;;  #  tree-head is not a list,it is a pair combined 
;;     @rank and @trees
;;     call to (car) or (cdr) to retrieve the element in a pair
;;     is legal except @pair is '()
(define (fibHeap-makeup-tree-head rank . trees)
  (cons rank trees)
  )

;;  fibHeap-tree-head-placeholder - no NIL is possible to appears in list
;;                                  as (cdr) that the position is not
;;                                  the expected end of list
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
;;  ERROR:                      the tree-heads have different rank
;;  #  combine is not "merge",this procedure does not merge
;;     the two tree-heads,just places the trees from two tree-list's
;;     in a box.
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
        (error "Try to combine two tree-heads have different rank."
               (fibHeap-tree-head-rank tree-head1)
               (fibHeap-tree-head-rank tree-head2)
               )
        )
    )
   )


(define (fibHeap-tree-head-modify-rank tree-head new-rank)
  (fibHeap-makeup-tree-head new-rank (fibHeap-tree-head-trees tree-head))
  )


;;  TREE-NODE


;;  fibHeap-node-lc-counter-max - maximum number of a node that lost
;;                                childs recently
;;                                
;;  #  a node is not root,then everytime it lose a child,
;;     node.lc-counter must be increased.
;;     the maximum value of the node.lc-counter is
;;     defined by @fibHeap-node-lc-counter-max,it equals to
;;     2.when node.lc-counter reach the max,have to cut it away
;;     to its parent(cascade cutting).
(define fibHeap-node-lc-counter-max 2)

;;  fibHeap-node-lc-counter-init - initial value is zero.
(define fibHeap-node-lc-counter-init 0)

;;  fibHeap-makeup-node - the node in the fibHeap-tree-head.
;;  @key:                 key value
;;  @childs:              list of childs
;;  @lc-counter:          count how many childs this node had been lost
;;  @r-bit:               state that indicates whether the node is 
;;                        a root node
(define (fibHeap-makeup-node key childs lc-counter r-bit)
  (list key childs lc-counter r-bit)
  )

(define fibHeap-null-node '())

(define (fibHeap-makeup-leaf key)
  (fibHeap-makeup-node key
                       '()
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

;;  fibHeap-node-decrease-key - node scoped DecreaseKey
;;  @node:                      the node to decrease
;;  @delta:                     delta value
;;  return:                     new node that had decreased
;;
;;  #  even if @delta is a negative number
;;     we stay substract @delta from it
(define (fibHeap-node-decrease-key node delta)
  (fibHeap-makeup-node
   (- (fibHeap-node-key node)
      delta)
   (fibHeap-node-childs node)
   (fibHeap-node-lc-counter node)
   (fibHeap-node-r-bit node)
   )
  )

;;  fibHeap-node-is-key-exist? - if a node contains specified key is
;;                               a descendant of a tree
;;  @key:                        key
;;  @root:                       ROOT of the tree may contains the node
;;  return:                      #f => no exist
;;                               #t => exist
(define (fibHeap-node-is-key-exist? key root)
  (define (scan-childs childs)
    (if (eq? fibHeap-null-node childs)
        #f
        (or (fibHeap-node-is-key-exist? key (car childs))
            (scan-childs (cdr childs))
            )
        )
    )
  (cond
   ((eq? fibHeap-null-node root)
    #f
    )
   ((= key (fibHeap-node-key root))
    #t
    )
   (else
    (scan-childs (fibHeap-node-childs root))
    )
   )
  )


;;  fibHeap-node-cascade-cut-off - cut off a node from a tree,and process a
;;                                 cascade cut off,if necessary
;;  @node-key:                     the key of node is going to cutting off
;;  @tree:                         the tree which contains the node
;;  return:                        a new forest is constructed by some new
;;                                 tree
;;
;;  suppose X is the node to cut off,P is parent of X,R is root of the tree
;;    cut-off X => a new tree owns X as its ROOT
;;    cascade-off P => a new tree owns P as its ROOT
;;    remainded R => a new tree owns R as its ROOT and
;;                   tree X,tree P are no longer existed
;;                   in tree R
;;  
;;  new-forest = { ... X ... P ... R ... }
(define (fibHeap-node-cascade-cut-off node-key tree)

  ;;  cut-off-descendant - the node have to be cut off is a descendant
  ;;                       of the tree
  ;;  @parent:             root of the tree to search
  ;;  return:              a new-forest is constructed as above
  ;;
  ;;  #  cut-off:
  ;;       X-forest { ... X ... }
  ;;     cascade cut-off:
  ;;       cascade-forest { ... X'parent ... }
  ;;     remain:
  ;;       @tree-forest { ... @tree - X - X'parent ... }
  ;;     finally:
  ;;       finally-forest { (merge X-forest (merge cascade-forest @tree-forest)) }
  (define (cut-off-descendant parent)

    ;;  enter-childs - call to procedure cut-off-descendant on each child
    ;;  @childs:       child list
    ;;  retunr:        a forest that merged each (cut-off-descendant child)
    ;;                 to return
    (define (enter-childs childs)
      (cond
       ((eq? fibHeap-null-node childs)
        fibHeap-null-forest
        )
       (else
        (fibHeap-merge (cut-off-descendant (car childs))
                       (enter-childs (cdr childs))
                       )
        )
       )
      )

    ;;  check-childs - check whether the node we want to cut off is exist in
    ;;                 child list of the parent node
    ;;  @childs:       the list of childs
    ;;  return:        #t => find out
    ;;                 otherwise #f
    (define (check-childs childs)
      (if (or (null? childs) (not (= node-key (fibHeap-node-key (car childs)))))
          (or #f (check-childs (cdr childs)))
          #t
          )
      )

    ;;  find-from-childs - retrieve the node contains @node-key from @childs
    ;;                     of current parent
    ;;  @childs:           child list
    ;;  return:            X => find out
    ;;                     NIL => not exist
    ;;                     (this case is able to be avoided through call to
    ;;                     (check-childs) before enter this procedure)
    (define (find-from-childs childs)
      (cond
       ((null? childs)
        fibHeap-null-node
        )
       ((= node-key (fibHeap-node-key (car childs)))
        (car childs))
       (else
        (find-from-childs (cdr childs))
        )
       )
      )

    ;;  unlink-X-from-childs - unlink X to childs
    ;;  @X:                    the node to unlink
    ;;  @childs:               child list
    ;;  return:                updated child list
    (define (unlink-X-from-childs X childs)
      (cond
       ((eq? fibHeap-null-node childs)
        '())
       ((= X (fibHeap-node-key (car childs)))  ;;  just skip it
        (cdr childs)
        )
       (else
        (cons (car childs)
              (unlink-X-from-childs X (cdr childs))
              )
        )
       )
      )

    (let
        ((childs (fibHeap-node-childs parent))
         )
      (if (not (check-childs childs))
          (enter-childs childs)
          (let*
              ((node (find-from-childs childs))  ;;  X
               (new-childs (unlink-X-from-childs node childs))
               (the-parent parent)
               )
            (let*
                ((node-tree-head (fibHeap-makeup-tree-head (fibHeap-node-calculate-rank node)
                                                           node)
                                 )
                 (node-forest (fibHeap-forest-place-th node-tree-head
                                                       fibHeap-null-forest)
                              )
                 (updated-parent (fibHeap-node-increase-lc-counter (fibHeap-makeup-node
                                                                    (fibHeap-node-key the-parent)
                                                                    new-childs
                                                                    (fibHeap-node-r-bit the-parent)
                                                                    )
                                                                   )
                                 )
                 )

              (if (fibHeap-node-should-cascade-cut-off? updated-parent)
                  (fibHeap-merge node-forest
                                 (fibHeap-node-cascade-cut-off (fibHeap-node-key updated-parent)
                                                               tree)
                                 )
                  (let
                      ((tree-tree-head (fibHeap-makeup-tree-head (fibHeap-node-calculate-rank tree)
                                                                 tree)
                                       )
                       )
                    (fibHeap-merge node-forest
                                   (fibHeap-forest-place-th tree-tree-head
                                                            fibHeap-null-forest)
                                   )
                    )
                  )
              )
            )
          )
      )
    )

  ;;  cut-off-root - the node we want to cut off is the ROOT of
  ;;                 @tree
  ;;  @tree:         the tree
  ;;  return:        a new forest constructed by
  ;;                 ROOT of @tree and childs of ROOT
  (define (cut-off-root tree)

    ;;  get-maximum-rank-from-tree-heads - find the maximum rank in 
    ;;                                     tree-head list
    ;;  @current:                          maximum rank saver
    ;;  @sequence:                         tree-head list
    ;;  return:                            @current
    (define (get-maximum-rank-from-tree-heads current sequence)
      (cond 
       ((null? sequence)
        current
        )
       ((> current (fibHeap-tree-head-rank (car sequence)))
        (get-maximum-rank (fibHeap-tree-head-rank (car sequence))
                          (cdr sequence)
                          )
        )
       (else
        (get-maximum-rank current (cdr sequence))
        )
       )
      )

    ;;  root - local variable represents
    ;;         ROOT of @tree
    (define root (fibHeap-makeup-node
                  (fibHeap-node-key tree)
                  '()
                  fibHeap-node-lc-counter-init
                  fibHeap-node-r-bit-on
                  )
      )

    ;;  childs-tree-head - local variable represents
    ;;                     a tree-head list about all
    ;;                     childs' tree-head
    (define childs-tree-head
      (map (lambda (child)
             (let
                 ((temp (fibHeap-node-reset-lc-counter
                         (fibHeap-node-modify-r-bit
                          child
                          fibHeap-node-r-bit-on
                          )
                         )
                        )
                  )
               (fibHeap-makeup-tree-head
                (fibHeap-node-calculate-rank temp)
                temp)
               )
             )
           (fibHeap-node-childs tree)
           )
      )

    (let
        ((root-tree-head (fibHeap-makeup-tree-head
                          (fibHeap-node-calculate-rank root)
                          root)
                         )
         )
      (let
          ((forest (fibHeap-forest-extend
                    (get-maximum-rank-from-tree-heads (fibHeap-tree-head-rank
                                                       root-tree-head
                                                       )
                                                      childs-tree-head
                                                      )
                    )
                   )
           )

        (define (deal-with-root)
          (set! forest
                (place-tree-head-on-proper-pos root-tree-head
                                               forest)
                )
          )
        (define (deal-with-childs-tree-head childs)
          (if (not (null? childs))
              (map (lambda (child)
                     (set! forest
                           (fibHeap-forest-place-th
                            child
                            forest)
                           )
                     )
                   childs)
              )
          )

        (deal-with-root)
        (deal-with-childs-tree-head childs-tree-head)
        forest
        )
      )
    )

  (cond
   ((eq? tree fibHeap-null-node)
    fibHeap-null-forest
    )
   ((= node-key (fibHeap-node-key tree))
    (cut-off-root tree)
    )
   (else
    (cut-off-descendant tree)
    )
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

(define (fibHeap-node-reset-lc-counter node)
  (fibHeap-makeup-node
   (fibHeap-node-key node)
   (fibHeap-node-childs node)
   fibHeap-node-lc-counter-init
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
   
;;  fibHeap-should-cascade-cut-off? - is the node should to be
;;                                    cut off?
;;  #  lc-counter = 2 AND !r-bit
;;     generally,when a node becomes root,its lc-counter
;;     would reset to zero.                                 
(define (fibHeap-node-should-cascade-cut-off? node)
  (and (= (fibHeap-node-lc-counter node)
          fibHeap-node-lc-counter-max)
       (not (fibHeap-node-r-bit node))
       )
  )

;;  fibHeap-node-account-descendants - account the number of
;;                                     descendants of a tree
;;  @node:                             node as the root of
;;                                     the tree
;;  return:                            how many descendants exist
;;
;;  #  descendants of a node is equal to the sum of descendants
;;     of its childs plus itself.
(define (fibHeap-node-account-descendants node)
  (define (traverse-all-to-account-descendants node result)
    (if (null? node)
        result
        (+ 1 (sequence_sum 
              (map (lambda (child) (traverse-all-to-account-descendants child 0))
                  (fibHeap-node-childs node)
                  )
              )
           )
        )
    )
  (traverse-all-to-account-descendants node)
  )

;;  fibHeap-node-calculate-rank - calculate rank of the tree on root
;;                                @node
;;  @node:                        the node as the root
;;  return:                       number of childs of the @node
(define (fibHeap-node-calculate-rank node)
  (sizeof_list (fibHeap-node-childs node))
  )

;;  forest start in rank0
;;  k := rank of tree
;;  place of tree in forest := rank_k

;;  OPERATIONS

;;  these ops should access to @Eva a predefined
;;  null forest

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
(define (fibHeap-findout-a-node key tree))

;;  fibHeap-DecreaseKey - decrease the key of @node
;;                        in @delta
;;  @forest:              forest contains the node
;;  @node_key:            the node contains the key
;;                        to be decreased
;;                        #  because we use the key
;;                           to represent the node
;;                           than specify a symbol
;;                           as the identifier.
;;
;;  @delta:               decrease value
;;  return:               a new forest which is updated
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
(define (fibHeap-DecreaseKey forest node_key delta))
  

;;  fibHeap-DeleteMin - delete the node contains a minimum
;;                      key in the forest
;;  @forest:            DeleteMin operate on
;;  return:             forest after DeleteMin done
;;  #  DeleteMin cause merges to all tree-heads they just
;;     added into forest by former Merges but
;;     have not been merged.
(define (fibHeap-DeleteMin forest))
           
           
                                    
      
