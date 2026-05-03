#lang racket
(provide execute)

;obal svg_tag
(define (make-svg widht height content)
    (format "<svg width=\"~a\" height=\"~a\">~a</svg>" widht height content)
)
;jednotlive utvary
(define (make-circle x y r style)
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style)
)
(define (make-rect x y widht height style)
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y widht height style)
)
(define (make-line x1 y1 x2 y2 style)
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style)
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
        [(? string?) expr]
        [(? symbol?) (lookup expr env)]
        [(? list?)  
            (cond
                ;jedlotive kreslici objekty
                [(eq? (car expr) 'circle) 
                    (make-circle (eval-expr (list-ref expr 1) env)
                                (eval-expr (list-ref expr 2) env)
                                (eval-expr (list-ref expr 3) env)
                                (eval-expr (list-ref expr 4) env)
                    )    
                ]
                [(eq? (car expr) 'line) 
                    (make-line (eval-expr (list-ref expr 1) env)
                                (eval-expr (list-ref expr 2) env)
                                (eval-expr (list-ref expr 3) env)
                                (eval-expr (list-ref expr 4) env)
                                (eval-expr (list-ref expr 5) env)
                    ) 

                ]
                [(eq? (car expr) 'rect)
                    (make-rect (eval-expr (list-ref expr 1) env)
                                (eval-expr (list-ref expr 2) env)
                                (eval-expr (list-ref expr 3) env)
                                (eval-expr (list-ref expr 4) env)
                                (eval-expr (list-ref expr 5) env)
                    ) 
                ]

                ;znamenka 
                [(eq? (car expr) '+)
                (apply + (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '-)
                (apply - (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '*)
                (apply * (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '/)
                (apply / (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) 'floor)
                (apply floor (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) 'cos)
                (apply cos (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) 'sin)
                (apply sin (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '=)
                (apply = (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '<)
                (apply < (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]

                [(eq? (car expr) '>)
                (apply > (map (lambda (arg) (eval-expr arg env)) (cdr expr)))]
                

                ; if a when 

                [(eq? (car expr) 'if)
                    (if (eval-expr (list-ref expr 1) env)
                        (eval-expr (list-ref expr 2) env)
                        (eval-expr (list-ref expr 3) env)
                    )
                ]

                [(eq? (car expr) 'when)
                    (if (eval-expr (list-ref expr 1) env)
                        (apply string-append
                            (map (lambda (arg) (format "~a" (eval-expr arg env))) (cddr expr)))
                        "")
                ]
                [else
                (let* (;; 1. Vytáhneme si jméno funkce (např. 'recur-circ) a její argumenty
                        [func-name (car expr)]
                        [args-exprs (cdr expr)]
                        
                        ;; 2. Vyhodnotíme předané argumenty (z čehokoliv se stanou reálná čísla/stringy)
                        [evaled-args (map (lambda (arg) (eval-expr arg env)) args-exprs)]
                        
                        ;; 3. Najdeme si definici funkce ve slovníku
                        ;; Z Kroku 4 víme, že výsledek je seznam: (list (parametry) (tělo))
                        [func-def (lookup func-name env)]
                        [params (car func-def)]    ;; např. '(x y r)
                        [body (cadr func-def)]     ;; např. '((circle ...) (when ...))
                        
                        ;; 4. MAGIE: Vytvoříme nové páry (parametr . hodnota)
                        ;; Funkce (map cons ...) umí geniálně vzít dva seznamy a sezipovat je do párů!
                        [new-bindings (map cons params evaled-args)]
                        
                        ;; Spojíme tyhle nové páry se starým prostředím
                        [new-env (append new-bindings env)])
                
                ;; 5. Vyhodnotíme celé tělo funkce v NOVÉM prostředí (stejně jako u 'when)
                (apply string-append
                        (map (lambda (b) 
                                (format "~a" (eval-expr b new-env))) 
                            body)))]
            )
        ]
    )



;-------------------------------------------------------
)
(define (execute width height prg expr)
  (let* (;; 1. Vytvoříme si globální slovník (prostředí) ze všech definic v 'prg'
         [global-env (build-global-env prg)]
         
         ;; 2. Spustíme tvůj nadupaný vyhodnocovač na hlavní výraz 'expr'.
         ;;    Ten se zanoří do rekurze a postupně vyplivne jeden dlouhý text s tvary.
         ;;    (Opět pro jistotu obalíme do format, kdyby to náhodou vrátilo číslo)
         [content (format "~a" (eval-expr expr global-env))])
    
    ;; 3. Vezmeme ten vygenerovaný vnitřek a obalíme ho do hlavního SVG tagu
    ;;    pomocí tvé funkce z Kroku 2, které předáme i šířku a výšku.
    (make-svg width height content)))
;---------------------------------------------------------




(define test2
  '((define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
    (define START 195)
    (define END 10)
    (define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2)))))))

(display (execute 400 400 test2 '(circles 200 START)))


