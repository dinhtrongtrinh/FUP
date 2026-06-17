; for testing
(define test-board
  (map string->list (string-split ".*..\n..*.\n**..\n...*\n*...")))

; for converting ints to chars.
(define (int->digit i) (integer->char (+ 48 i)))

(define (valid? x y board)
    (let ([boardH (length board)]
        [boardW (length (car board))])
        (cond [(and (and (>= x 0) (< x boardW )) (and (>= y 0) (< y boardH))) #t]
            [else #f] 
        )
    )
)

(define combination
  '((-1 -1) (-1 0) (-1 1)
    (0 -1)          (0 1)
    (1 -1)  (1 0)  (1 1)))


(define (get-negh x y board)
    (let* ([allCombi (map (lambda (combi) (list (+ x (car combi)) (+ y (cadr combi))))combination)]
        [validPos (filter (lambda (pos) (list (valid? (car pos) (cadr pos) board))) allCombi)]
    ))
)

(define (count-mines validPos board)
    (length (filter
            (lambda (znak) (char=? znak #\*)) (map (lambda (pos) (list-ref (list-ref board (cadr y)) (car x)))) validPos))
)
(define (charRule ch val)
    (cond [(equal? ch #\*) #\*]
        [(equal? val 0) #\.]
        [else (int->digit val)]
    )
)
(define (sweep board)
  (let ([h (length board)]
        [w (length (car board))])
    ; Vnější cyklus projde všechny řádky 'y' od 0 do h-1
    (for/list ([y (in-range h)])
      ; Vnitřní cyklus projde všechny sloupce 'x' od 0 do w-1
      (for/list ([x (in-range w)])
        ; --- TADY JE TVŮJ ÚKOL ---
        ; 1. Vytáhni původní znak na pozici (x, y) z 'board'
        ; 2. Získej seznam sousedů pomocí tvé funkce (get-negh x y board)
        ; 3. Spočítej miny pomocí tvé funkce (count-mines sousedi board)
        ; 4. Zavolej (charRule puvodni-znak pocet-min)
        (charRule (list-ref (list-ref board y) x) (count-mines (get-negh x y board) board))
        ))))

(let* ((input-string (port->lines))
        ; implement parsing of board/sweep for mines 
        ; assuming counted-board contins a list of list of chars
       (sn (map list->string counted-board)))
  (for ((l sn))
    (display l)
    (newline)))