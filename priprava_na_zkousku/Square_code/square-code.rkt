#lang racket
(provide encode)

(define (normalize str)
  (let ([onlyAlpha (filter char-alphabetic? (string->list str))])
    (string-downcase (list->string onlyAlpha))
   )
)
(define (fillLastLine strList c)
  (if (< (length strList) c)
      (fillLastLine (cons #\space strList) c)
      (reverse strList)
  )
)

(define (squareUp str)
  (let* ([strList (string->list str)]
         [lenStr (length strList)]
        [c (exact-ceiling(sqrt lenStr))])
    
    (define (recurs strList [acc '()])
      (if (<= (length strList) c)
          (reverse (cons (fillLastLine(reverse strList)c) acc))
          (recurs (drop strList c) (cons (take strList c) acc))
      )
    )
    (recurs strList)
  )
)
(define (transpose strList)
  (apply map list strList)
)

(normalize "If man was meant to stay on the ground, god would have given us roots.")
(transpose(squareUp "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots"))

(define (encode str)
  (let* ([normal (normalize str)]
         [square (squareUp normal)]
         [trans (transpose square)]
         [filterDot (map (lambda (x) (filter char-alphabetic? x)) trans)]
         [trasnStr (map list->string trans)])
    (string-join trasnStr)
  )
)
(encode "If man was meant to stay on the ground, god would have given us roots.")
(encode "Have a nice day!")
; your code here