#||
 | Maximum sub-sequence sum
 | There has three algorithms to solve this problem.
 |#

#||
 | 1>
 |   procedure max-subseq-sum({An}: input set, n: number of elements)
 |     i := 0 j := 0 k := 0
 |     Maxsum := 0 Thissum := 0
 |     for i := 0 to n - 1
 |       for j := i to n - 1
 |         Thissum := 0
 |         for k := i to j
 |           Thissum := Thissum + Ak
 |         if Thissum > Maxsum
 |           Maxsum := Thissum
 |     return Maxsum
 |
 |   Time Complexity => O(N^3)
 |#
(define (maximum-subsequence-sum1 seq)
  (define (seq-from-i seq i)
    (if (or (null? seq) (= i 0))
        seq
        (seq-from-i (cdr seq) (- i 1))
        )
    )
  (define (seq-end-k seq k)
    (if (or (null? seq) (= k 0))
        '()
        (cons (car seq) (seq-end-k (cdr seq) (- k 1)))
        )
    )
  (define (subseq seq start end)
    (seq-end-k (seq-from-i seq start) end)
    )

  (define (for-2 seq Thissum)
    (if (null? seq)
        Thissum
        (for-2 (cdr seq) (+ Thissum (car seq)))
        )
    )

  (define (for-1 seq current-i Maxsum)
    (define (for-1-helper seq j Maxsum)
      (if (null? seq)
          Maxsum
          (let*
              ((for-2-seq (subseq seq current-i j))
               (for-2-ret (for-2 for-2-seq 0))
               (new_Maxsum (if (< Maxsum for-2-ret)
                               for-2-ret
                               Maxsum
                               ))
               )
            (for-1-helper (cdr seq) (+ j 1) new_Maxsum)
            )
          )
      )
    (for-1-helper (seq-from-i seq current-i) current-i Maxsum)
    )

  (define (for-0 seq i Maxsum)
    (if (null? seq)
        Maxsum
        (for-0 (cdr seq) (+ i 1)
               (for-1 seq i Maxsum)
               )
        )
    )

  (for-0 seq 0 0)
  )

#||
 | 2>
 |   procedure max-subseq-sum({An}: input set, n: number of elements)
 |     Thissum := 0 Maxsum := 0
 |     i := 0 j := 0
 |     for i := 0 to n - 1
 |       Thissum := 0
 |       for j := i to n - 1
 |         Thissum := Thissum + Aj
 |       if Thissum > Maxsum
 |         Maxsum := Thissum
 |     return Maxsum
 |
 |   Time Complexity => O(N^2)
 |#
(define (maximum-subsequence-sum2 seq)
  (define (seq-from-i seq i)
    (if (or (null? seq) (= i 0))
        seq
        (seq-from-i (cdr seq) (- i 1))
        )
    )

  (define (for-0 seq i Maxsum)
    (if (null? seq)
        Maxsum
        (let*
            ((for-1-seq (seq-from-i i))
             (for-1-ret (for-1 for-1-seq 0))
             (new_Maxsum (if (< Maxsum for-1-ret)
                             for-1-ret
                             Maxsum
                             ))
             )
          (for-0 (cdr seq) (+ i 1) new_Maxsum)
          )
        )
    )

  (define (for-1 seq Thissum)
    (if (null? seq)
        Maxsum
        (for-1 (cdr seq) (+ Thissum (car seq)))
        )
    )

  (for-0 seq 0 0)
  )

#||
 | 3>
 |   procedure max-sub-sum({An}: input set, Left: left border, Right: right border)
 |     if Left == Right
 |       if Aleft > 0
 |         return Aleft
 |       else
 |         return 0
 |     center := (Left + Right) / 2
 |     maxLeftSum := max-sub-sum(An, Left, center)
 |     maxRightSum := max-sub-sum(An, center + 1, Right)
 |     
 |     maxLeftBorderSum := 0
 |     LeftBorderSum := 0
 |     for i := center to Left
 |       LeftBorderSum := LeftBorderSum + Ai
 |       if LeftborderSum > maxLeftBorderSum
 |         maxLeftBorderSum := LeftBorderSum
 |
 |     maxRightBorderSum := 0
 |     RightBorderSum := 0
 |     for i := center + 1 to right
 |       RightBorderSum := RightBorderSum + Ai
 |       if RightBorderSum > maxRightBorderSum
 |         maxRightBorderSum := RightBorderSum
 |
 |     return maxs(maxLeftSum, maxRightSum, maxLeftBorderSum + maxRightBorderSum)
 |
 |   Time Complexity => O(NlogN)
 |#
(define (maximum-subsequence-sum3 seq)
  (define (seq-from-i seq i)
    (if (or (null? seq) (= i 0))
        seq
        (seq-from-i (cdr seq) (- i 1))
        )
    )
  (define (seq-end-k seq k)
    (if (or (null? seq) (= k 0))
        '()
        (cons (car seq) (seq-end-k (cdr seq) (- k 1)))
        )
    )
  (define (subseq seq start end)
    (seq-end-k (seq-from-i seq start) end)
    )
  (define (seq-length seq length)
    (if (null? seq)
        0
        (seq-length (cdr seq) (+ length 1))
        )
    )
  (define (call-seq-length seq) (seq-length seq 0))

  (define (max-sub-sum seq)
    (cond ((null? seq) 0)
          ((= 1 (call-seq-length seq))
           (if (> (car seq) 0)
               (car seq)
               0
               )
           )
          (else
           (let*
               (define (bin-split-seq seq)
                 (cond ((= 1 (call-seq-length seq))
                        (cons seq '())
                        )
                       ((= 2 (call-seq-length seq))
                        (cons (cons (car seq) '())
                              (cdr seq)
                              )
                        )
                       (else
                        (let
                            ((mid (ceiling (/ (call-seq-length seq) 2))))
                          (cons (subseq seq 0 mid)
                                (seq-from-i seq mid)
                                )
                          )
                        )
                       )
                 )
             (define (max-sum seq currentv maxv)
               (if (null? seq)
                   maxv
                   (let*
                       ((new_cv (+ currentv (car seq)))
                        (new_maxv (if (< maxv new_cv)
                                      new_cv
                                      maxv)
                                  ))
                     (max-sum (cdr seq) new_cv new_maxv)
                     )
                   )
               )

             ((splited-seq (bin-split-seq seq))
              (left-seq (car splited-seq))
              (right-seq (cdr splited-seq))
              (left-maxsum (max-sum left-seq 0 0))
              (right-maxsum (max-sum right-seq 0 0))
              (lmax (max-sub-sum left-seq))
              (rmax (max-sub-sum right-seq))
              )
             (max lmax, rmax, (+ left-maxsum right-maxsum))
             )
           )
          )
    )

  (max-sub-sum seq)
  )

#||
 | for each element in @seq
 |     current sum := current sum + current element
 |     if current sum > maximum sum
 |         let maximum sum := current sum
 |     if current sum < 0
 |         reset current sum to 0
 |#
(define (maximum-subsequence-sum4 seq)
  (define max-sum 0)
  (define (for-each current the-seq)
    (if (not (null? the-seq))
        (let
            ((new-sum (+ current (car the-seq))))
          (if (> new-sum max-sum)
              (set! max-sum new-sum))
          (if (< new-sum 0)
              (for-each 0 (cdr the-seq))
              (for-each new-sum (cdr the-seq)))
          )
        '())
    )
  (for-each 0 seq)
  max-sum)





          
      
      



