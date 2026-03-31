#lang racket
(provide execute)

;obal svg_tag
(define (make-svg widht height content)
    (format "<svg width=\"~a\" height=\"~a\"> ~a </svg>" widht height content)
)
;jednotlive utvary
(define (make-circle x y r style)
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style)
)
(define (make-rect x y widht height style)
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>")
)
(define (make-line x1 y1 x2 y2 style)
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>")
)

;lookup
(define (lookup name env)
    (cdr (assoc name env))
)

; checkonavni env
(define (check-function-or-constant x)
    (if (list? (cadr x)) 
    ;; ANO, druhý prvek je seznam, takže je to FUNKCE
    (let ([jmeno (car (cadr x))]
          [argumenty (cdr (cadr x))]
          [telo (cddr x)])
      (cons jmeno (list argumenty telo)))
    
    ;; NE, druhý prvek není seznam, takže je to KONSTANTA
    (let ([jmeno (cadr x)]
          [hodnota (caddr x)])
      (cons jmeno hodnota)))
)

(define (build-global-env prg)
    (map check-function-or-constant prg)
)

(define (eval-expr expr env)
    (match expr
        [(? number?) expr]
        [(? symbol?) (lookup expr env)]
        [(? list?)  
            (cond
                [(eq? (car expr) 'circle) 
                    (make-circle (lookup (list-ref expr 1) env) (lookup (list-ref expr 2) env) (lookup (list-ref expr 3) env) (lookup (list-ref expr 4) env))    
                ]
                [(eq? (car expr) 'line) 
                    (make-line (lookup (list-ref expr 1) env) (lookup (list-ref expr 2) env) (lookup (list-ref expr 3) env) (lookup (list-ref expr 4) env) (lookup (list-ref expr 5) env))    
                ]
                [(eq? (car expr) 'rect)
                    (make-rect (lookup (list-ref expr 1) env) (lookup (list-ref expr 2) env) (lookup (list-ref expr 3) env) (lookup (list-ref expr 4) env) (lookup (list-ref expr 5) env))    
                ]
                [else
                    ((car expr) (lookup (list-ref expr 1) env) (lookup (list-ref expr 2) env))
                ]
            )
        ]
    )
)
(define (execute width height prg expr)
""
)
(define test-env '((x . 100) (y . 50) (STYLE . "fill:blue")))
(eval-expr 42 test-env)
(eval-expr 'x test-env)
(eval-expr '(circle x (- y 20) 30 STYLE) test-env)

