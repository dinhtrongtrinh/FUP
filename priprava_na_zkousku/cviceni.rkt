#lang racket

(define (sum-to n [acc 0])
    (if (> n 0)
        (sum-to (- n 1) (+ acc n))
        acc
    )
)
;(sum-to 5)
(define (fib n [a 0] [b 1])
    (cond 
    [(= n 1) a]
    [(= n 2) b]
    [(> n 2) (fib (- n 1) b (+ a b))]
    )
)
;(fib 6)
(define (my-lenght lst [acc 0])
    (if (empty? lst)
        acc
        (my-lenght (rest lst) (+ acc 1))    
    )
)
(define (word alp n)
  (cond [(equal? n 0) '()]
         [(equal? n 1) (map list alp)]
         [else (apply append
                      (map (lambda (x)
                             (map (lambda (a) (cons a x))alp))
                           (word alp (- n 1))))]
  )
)
(word '(#\a #\b) 3)


