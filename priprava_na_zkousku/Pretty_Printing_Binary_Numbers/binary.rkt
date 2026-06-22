#lang racket
(define img-zero
  '(".##."
    "#..#"
    "#..#"
    ".##."))

(define img-one
  '("...#"
    "..##"
    "...#"
    "...#"))
(define (toBin n)
  (cond [(equal? n 0) '(0)]
        [else
         (define (go x [acc '()])
           (cond [(equal? x 0) acc]
                 [else (go (quotient x 2) (cons (if (equal? (modulo x 2) 1) 1 0) acc)) ]
           )
         )
         (go n)
        ]
  )
)
(define (oneLayer n pos)
  (cond [(equal? pos 0) (string-join (foldr (lambda (x acc) (cons (if (equal? x 1) (car img-one) (car img-zero)) acc)) '() n) ".")]
        [(equal? pos 1) (string-join (foldr (lambda (x acc) (cons (if (equal? x 1) (cadr img-one) (cadr img-zero)) acc)) '() n) ".")]
        [(equal? pos 2) (string-join (foldr (lambda (x acc) (cons (if (equal? x 1) (caddr img-one) (caddr img-zero)) acc)) '() n) ".")]
        [(equal? pos 3) (string-join (foldr (lambda (x acc) (cons (if (equal? x 1) (cadddr img-one) (cadddr img-zero)) acc)) '() n) ".")]

  )
)

(define (binToImg n)
  (string-join (map (lambda (x) (oneLayer n x)) '(0 1 2 3)) "\n")
)

(display "Enter integer:\n")
(define n (read-line))
(display (binToImg (toBin (string->number n))))


