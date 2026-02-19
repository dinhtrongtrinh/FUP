#lang racket

(define (my-even? n)
    (cond
    [(= n 1) "Is odd\n"]
    [(= n 0) "Is even\n"]
    [else (my-even? (- n 2))]))

(define (string-repeat n s)
  (if (> n 0) 
      (begin   
        (display s)
        (string-repeat (- n 1) s))
      (void))) 

;(string-repeat 3 "cuka bylate ")


(define (num-of-digits n a)
    (if (not (= n 0))
        (num-of-digits (quotient n 10) (+ a 1))
        a))
;(display(num-of-digits -16454324 0))

(define (num->str n [radix 10])
    (if (= radix 10)
        n
        (begin
            (cond
                []
            )
        )
    )

)


;(display (my-even? 14))

