#lang racket
(provide grid)

(define (larger pointA pointB)
  (if (< (+ (car pointA) (cadr pointA)) (+ (car pointB) (cadr pointB)))
      pointB
      pointA
  )
)
;(larger '(1 3) '(0 0)) 

(define (largestPoint points [acc '(0 0)])
  (if (empty? points)
      acc
      (let ([currPoint (car points)])
        (largestPoint (cdr points) 
                      (larger (list (cadr currPoint) (caddr currPoint)) acc)))))

(define (calcDis pointA pointB)
  (+ (abs(- (car pointA) (car pointB))) (abs(- (cadr pointA) (cadr pointB))))
)
;(calcDis '(1 3) '(4 5))

(define (closestPoint target points [acc '(#\A 99 99)])
  (if (empty? points)
      acc
      (let ([currPoint (car points)])
        (if (< (calcDis target (list (cadr currPoint) (caddr currPoint))) (calcDis target (list (cadr acc) (caddr acc))))
            (closestPoint target (cdr points) currPoint)
            ;(closestPoint target (cdr points) acc)
            (if (equal? (calcDis target (list (cadr currPoint) (caddr currPoint))) (calcDis target (list (cadr acc) (caddr acc))))
                (closestPoint target (cdr points) (list #\. (cadr acc) (caddr acc)))
                (closestPoint target (cdr points) acc)
            )
        )
      )
  )
)
(define (matchPoint target points [acc #\-])
  (if (empty? points)
      acc
      (let ([currPoint (car points)])
        (if (equal? target (list (cadr currPoint) (caddr currPoint)))
            (matchPoint target (cdr points) (car currPoint))
            (matchPoint target (cdr points) acc)
        )
)))


(define (grid points)
  (let ([h (+ (cadr (largestPoint points)) 1)]
        [w (+ (car (largestPoint points)) 1)])
    (for/list ([y (in-range h)])
      (list->string(for/list ([x (in-range w)])
        (if (equal? (matchPoint (list x y) points) #\-) 
            (char-downcase (car (closestPoint (list x y) points)))
            (matchPoint (list x y) points)
        ))
      )
     )
   )
)

(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))
(car (closestPoint '(3 5) points))
(grid points)


