#lang racket
(define coeffs '(2 -1 3))
(define m1 '((1 2 3) (1 0 1) (0 2 0)))

(define (multi-each matrix-row num)
    (map (lambda (x) (* x num)) matrix-row))

(define (multi-row matrix vec)
    (map (lambda (row num) (multi-each row num))matrix vec))

(define (linear-combination matrix vec)
  (let ([multiplied-matrix (multi-row matrix vec)])
    (apply map + multiplied-matrix)))
    

;(display (linear-combination m1 coeffs))
(define p '(A 65))
(display p)
    

    