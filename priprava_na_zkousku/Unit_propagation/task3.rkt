#lang racket
(provide propagate-units (struct-out pos) (struct-out neg))

(struct pos (variable) #:transparent)
(struct neg (variable) #:transparent)


(define (findUnit listLiteral)
  (if (empty? listLiteral)
      #f
      (if (equal? (length (car listLiteral)) 1)
          (car (car listLiteral))
          (findUnit (cdr listLiteral))
      )
  )
)
;(findUnit '())
;(findUnit (list (list (pos "x") (pos "y")) (list (neg "z") (pos "w"))))
;(findUnit (list (list (pos "x")) (list (neg "y") (pos "z"))))
;(findUnit (list (list (pos "x") (neg "y")) (list (pos "z") (pos "w")) (list (neg "a"))))

(define (negateLiteral lite)
  (if (pos? lite)
      (neg (pos-variable lite))
      (pos (neg-variable lite))
  )
)
;(negateLiteral (pos "x"))



(define (cleanFormula lite listClause)
  (let ([clenedClause (filter (lambda (x) (not (member lite x))) listClause )])
    (map (lambda (clause) (filter (lambda (x) (not (equal?  x (negateLiteral lite)))) clause)
          )clenedClause)
  )
)
; Definice zkratek, ať to nemusíš otrocky rozepisovat
(define a (pos "a")) (define b (pos "b")) (define -b (neg "b"))
(define c (pos "c")) (define -c (neg "c")) (define e (pos "e")) (define -f (neg "f"))
(define x (pos "x")) (define -x (neg "x")) (define y (pos "y"))

; Formule pro testy
(define f1 (list (list x) (list -x y)))
(define f2 (list (list a) (list a b)))
(define f3 (list (list b c)))
(define f4 (list (list a b -c -f) (list b c) (list -b e) (list -b)))

(cleanFormula x f1)

> (define (propagate-units listlist)
    (cond [(equal? (findUnit listlist) #f) listlist]
          [else 
           (propagate-units (cleanFormula (findUnit listlist) listlist))
          ]

    )
)
> (propagate-units (list (list (pos "a") (pos "b") (neg "c") (neg "f"))
                         (list (pos "b") (pos "c"))
                         (list (neg "b") (pos "e"))
                         (list (neg "b"))))
> (propagate-units (list (list (pos "x")) (list (neg "x")) (list (pos "y")) (list (neg "y"))))
'(())
                   
; your code here