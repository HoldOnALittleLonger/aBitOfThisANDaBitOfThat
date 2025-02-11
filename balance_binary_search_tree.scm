

(define (bbst)

  (define null-tree '())
  (define (is-null-tree? t) (eq? null-tree t))

  #| for lazy-deleting |#
  (define tree-living 1)
  (define tree-dead 0)

  (define local-tree-object null-tree)

  (define (makeup-node-routine key left right flag)
    (list key left right flag)
    )
  (define (makeup-node key left right)
    (makeup-node-routine key left right
                         tree-living
                         )
    )

  (define (key t) (car t))
  (define (left t) (cadr t))
  (define (right t) (caddr t))
  (define (flag t) (cadddr t))

  (define (list-rest-from-Nth l n)
    (cond
     ((or (eq? l '()) (= n 0))
      l
      )
     ((< n 0)
      (error "Incorrect idx " n)
      )
     (else
      (list-rest-from-N (cdr l) (- n 1))
      )
     )
    )

  (define (list-modify-Nth-e l new n)
    (cond
     ((eq? l '())
      l
      )
     ((or (< n 0))
      (error "Incorrect idx " n)
      )
     ((= n 0)
      (cons new (cdr l))
      )
     (else
      (cons (car l)
            (list-modify-Nth-e (cdr l) new (- n 1))
            )
      )
     )
    )

  (define (list-Nth-v l n)
    (cond
     ((eq? l '())
      l
      )
     ((= n 0)
      (car l)
      )
     (else
      (list-Nth-v l (- n 1))
      )
     )
    )

  #||
   | convert-tree-to-list - convert a tree to a list in order lower-to-greater
   | @t:                    the tree
   | return:                list in order lower-to-greater
   |                        null list if null tree
   | # this just a extending for tree "inorder traversal".
   |#
  (define (convert-tree-to-list t)
    (if (is-null-tree? t)
        t
        (let
            ((left-list (convert-tree-to-list (left t)))
             (right-list (convert-tree-to-list (right t)))
             )
          (cond 
           ((eq? (flag t) tree-living)
            (if (eq? left-list '())
                (append (cons (key t) '())
                        (right-list)
                        )
                (append (append left-list
                                (cons (key t) '())
                                )
                        right-list)
                )
            )
           (else
            (append left-list right-list)
            )
           )
          )
        )
    )


  #||
   | make-balanced-tree-from-list - makeup a balanced binary search tree from
   |                                a specified list
   | @l:                            the list
   | return:                        balanced binary tree
   | # everything is simple.
   |   if we have a list @l in order 'lower-to-greater',then we can pick up the
   |   element @e which is in the middle position of this list.
   |   @e as a root node,furthermore,we can cut @l to two parts each is a sub-list.
   |   now,we invoke this procedure on both them,left-side as left-child of root,
   |   right-side as right-child of root.
   |   after recursion stopped,we get a balanced binary search tree.
   |#
  (define (make-balanced-tree-from-list l)
    (if (eq? l '())
        null-tree
        (let*
            ((root-idx (floor (/ (length l) 2)))
             (root (list-Nth-v l root-idx))
             (left-list (list-modify-Nth-e l '() root-idx))
             (right-list (list-rest-from-Nth l (+ 1 root-idx)))
             (left-child (make-balanced-tree-from-list left-list))
             (right-child (make-balanced-tree-from-list right-list))
             )
          (makeup-node root left-child right-child)
          )
        )
    )

    
  (define (find key)
    (define (find-routine t)
      (cond
       ((is-null-tree? t)
        (display "Unfound " key)
        (newline)
        t
        )
       ((and (= (key t) key)
             (eq? (flag t) tree-living)
             )
        t
        )
       ((< key (key t))
        (find-routine (left t))
        )
       (else
        (find-routine (right t))
        )
       )
      )

    (find-routine local-tree-object)
    )

  #||
   | procedures change a tree should invoke (set!) procedure on
   | @local-tree-object to set it with the result of routine.
   |
   |#
  ;; (define (insert key) ...)
  ;; (define (delete key) (define (lazy-deleting t) ...) ...)
  ;; (define (dispatch comm) ...)
)
