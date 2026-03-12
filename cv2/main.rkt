#lang racket
(require racket/trace)
(require 2htdp/image)


(define (my-reverse lst [agg '()])
    (if (empty? lst) 
        agg
        (my-reverse (cdr lst) (cons (car lst)agg))))

;(my-reverse '(a b c))
(define (list-average lst [sum 0] [size 0])
    (if (empty? lst)
        (display (/ sum size))
        (list-average (cdr lst) (+ sum (car lst)) (+ size 1))))

;(list-average '(2 4 6 8))

(define (list-split n lst [counter 0] [answer '()] [inside '()])
    (cond
        [(empty? lst)
            (if (empty? inside)
                answer
                (cons inside answer))]
        [(>= counter n)
            (list-split n
                        lst
                        0
                        (cons inside answer)
                        '())]
        [else
            (list-split n
                        (cdr lst)
                        (+ counter 1)
                        answer
                        (cons (car lst) inside))]))
;(trace list-split)
;(list-split 2'(34 80 8 0 6 ))





