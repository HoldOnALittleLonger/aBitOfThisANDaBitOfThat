#||
 | Fast Power :
 |   faster than normal power computing,because 
 |     v^power =>
 |                if power % 2 == 0
 |                    v^power => (v^2)^(power / 2)
 |                else
 |                    v^power => v^(power - 1) * v
 |                               =>  v^((power - 1) / 2) * 
 |                                   v^((power - 1) / 2) *
 |                                   v
 |   Time complexity : O(logN)
 |#

(define (fast-power v power)
  (cond
   ((= power 0)
    1)
   ((= power 1)
    v)
   ((= 0 (remainder power 2))
    (fast-power (* v v) (/ power 2)))
   (else
    (* (fast-power (* v v) (/ (- power 1) 2)) v))
   )
  )

