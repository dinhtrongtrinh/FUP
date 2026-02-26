#lang racket

(define (my-reverse lst [agg '()])
    (if (empty? lst) 
        agg
        (my-reverse (cdr lst) (cons (car lst)agg))))

;(my-reverse '(a b c))
(define (list-average lst [sum 0] [size 0])
    (if (empty? lst)
        (display (/ sum size))
        (list-average (cdr lst) (+ sum (car lst)) (+ size 1))))
(list-average '(2 4 6 8))


