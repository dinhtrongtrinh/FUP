#lang racket
(provide spiral-matrix)

(define (addNum spiral)
  (let* ([len (+ (length spiral)2)]
         [num (- (* 4 len) 4)])
    (map (lambda (x) (map (lambda (number)
         (+ num number)) x)) spiral)
  )
)
(define (build-middle left-col B right-col)
  (map (lambda (l row r) 
         (append (list l) row (list r))) 
       left-col B right-col))

(define (buildTB top-row B down-row)
  (let ([top (cons top-row B)])
    (append top (list down-row))
  )
    
)


(define (spiral-matrix n [acc 3] [spiral '((1 2 3)(8 9 4)(7 6 5))])
  (cond [(equal? n 1) '((1))]
        [(equal? n 3) spiral]
        [(< 3 n) (if (equal? acc n)
                     spiral
                     (let* ([tempN (+ acc 2)]
                            [B (addNum spiral)]
                            [left-col (range (- (* 4 tempN) 4) (- (* 3 tempN) 2) -1)]
                            [right-col (range (+ tempN 1) (- (* 2 tempN) 1))]
                            [top (range 1 (+ tempN 1))]
                            [down (range (- (* 3 tempN) 2) (- (* 2 tempN) 2) -1)]
                            [buildMid (build-middle left-col B right-col)]
                            [buildtd (buildTB top buildMid down)])
                       (spiral-matrix n tempN buildtd)
                      )
                 )
       ]

    )
)
(spiral-matrix 11)
; your code here