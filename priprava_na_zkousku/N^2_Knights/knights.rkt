#lang racket


(define legal-moves '((-1 -2) (-2 -1) (-2 1) (-1 2) (1 2) (2 1) (2 -1) (1 -2)))
(define board '((1 0 0 0)
                (0 0 0 1)
                (1 0 0 0)
                (0 1 0 0)))
;zjistujeme validne pozice
(define (validPos x y board)
    (let ([H (length board)]
        [W (length (car board))])
        (cond [(and (< x W) (>= x 0) (< y H) (>= y 0)) #t]
            [else #f])
    )
)
;tady zjistime sousedy
(define (get-negh x y board)
  (filter (lambda (pos) (validPos (car pos) (cadr pos) board)) (map (lambda (combi) (list(+ x (car combi))(+ y (cadr combi)))) legal-moves))
)
;(get-negh 0 0 board)

;tady zjistime, jestli to uz neni soused
(define (canAttack? x y board)
  (let* ([posibleM (get-negh x y board)]
         [inDanger (filter (lambda (z) (equal? z 1)) (map (lambda (pos) (list-ref (list-ref board (cadr pos)) (car pos))) posibleM))])
    (if (empty? inDanger)
        #f
        #t
        )
  )
)
(define (isKnight? x y board)
  (if (equal? 1 (list-ref (list-ref board y) x))
      #t
      #f
  )
)

;(canAttack? 0 0 board)
(define (is_valid? board)
  (let ([H (length board)]
        [W (length (car board))])
    (for/and ([y (in-range H)])
      (for/and ([x (in-range W)])
        (cond [(isKnight? x y board) (not(canAttack? x y board))]
              [else #t]
        )
       )
    )
))
; 1. TESTOVACÍ DESKA: Všichni jezdci jsou v bezpečí (mělo by vrátit #t)
(define deska-ok
  '((0 0 0 0)
    (0 1 0 0)   ; 1 znamená jezdec
    (0 0 0 0)
    (0 0 0 0)))

; 2. TESTOVACÍ DESKA: Jezdci se navzájem ohrožují (mělo by vrátit #f)
; (Jezdec na [1, 1] ohrožuje jezdce na [2, 3] v L-tvaru)
(define deska-chyba
  '((0 0 0 0)
    (0 1 0 0)   
    (0 0 0 0)
    (0 0 1 0)))
(is_valid? deska-ok)    ; Sleduj, jestli ti DrRacket vypíše #t
(is_valid? deska-chyba) ; Sleduj, jestli ti DrRacket vypíše #f

(is_valid? board)



