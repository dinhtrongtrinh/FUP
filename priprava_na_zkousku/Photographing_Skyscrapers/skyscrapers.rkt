#lang racket

(provide best-view)

(define (rot grid)
  (apply map list (reverse grid)))

(define (checkRoofLine list [acc 1])
  (if (empty? (cdr list))
      acc
      (cond [(< (car list) (cadr list))
             (checkRoofLine (cdr list) (+ acc 1))
            ]
            [else acc]
      )
  )
)

(define (countRoofs grid)
  (foldl + 0 (map checkRoofLine grid))
)

(define city
  '((3 0 3 7 3)
    (2 5 5 1 2)
    (6 5 3 3 2)
    (3 3 5 4 9)
    (3 5 3 9 0)))
(define city2
  '((3 3 3)
    (1 2 3)
    (1 2 3)))

(require racket/list) ; Nutné pro funkci argmax

(define (best-view city)
  (let* ([W (countRoofs city)]
         [S (countRoofs (rot city))]
         [E (countRoofs (rot (rot city)))]
         [N (countRoofs (rot (rot (rot city))))]
         ;; Vytvoříme seznam párů (Symbol . Číslo)
         [vysledky (list (cons 'W W)
                         (cons 'S S)
                         (cons 'E E)
                         (cons 'N N))])
    
    ;; argmax vybere ten pár, který má na pozici 'cdr' (číslo) nejvyšší hodnotu
    (argmax cdr vysledky)))