#lang racket
(provide grid)

;; Pomocná funkce, která porovná dva body podle součtu jejich souřadnic (X + Y)
;; Vrací ten bod, který má tento součet větší.
(define (larger pointA pointB)
  (if (< (+ (car pointA) (cadr pointA)) (+ (car pointB) (cadr pointB)))
      pointB ; Pokud je B větší než A, vrátí pointB
      pointA ; Jinak vrátí pointA
  )
)
;(larger '(1 3) '(0 0)) 

;; Funkce projde seznam bodů a najde ten, který má největší součet souřadnic (X + Y).
;; Používá se k určení maximální velikosti mřížky (pravý dolní roh).
;; Vstupní body jsou ve formátu '(#\A X Y).
(define (largestPoint points [acc '(0 0)])
  (if (empty? points)
      acc ; Když projdeme všechny body, vrátíme akumulátor s největšími souřadnicemi
      (let ([currPoint (car points)])
        (largestPoint (cdr points) 
                      ;; Z aktuálního bodu '(#\A X Y) vytáhne X a Y a porovná s acc
                      (larger (list (cadr currPoint) (caddr currPoint)) acc)))))

;; Vypočítá manhattanskou vzdálenost mezi dvěma body A a B
;; Vzorec: |A.x - B.x| + |A.y - B.y|
(define (calcDis pointA pointB)
  (+ (abs(- (car pointA) (car pointB))) (abs(- (cadr pointA) (cadr pointB))))
)
;(calcDis '(1 3) '(4 5))

;; Pro zadaný cílový bod (target) najde v seznamu 'points' ten nejbližší bod.
;; Výchozí akumulátor je nastaven na fiktivní bod s obrovskou vzdáleností.
(define (closestPoint target points [acc '(#\A 99 99)])
  (if (empty? points)
      acc ; Všechny body prošeny, vracíme ten nejbližší
      (let ([currPoint (car points)])
        ;; Porovná vzdálenost k aktuálnímu bodu se vzdáleností k bodu v akumulátoru
        (if (< (calcDis target (list (cadr currPoint) (caddr currPoint))) 
               (calcDis target (list (cadr acc) (caddr acc))))
            ;; AKTIVNÍ BOD JE BLÍŽ: Pokračujeme rekurzí a aktuální bod se stává novým acc
            (closestPoint target (cdr points) currPoint)
            
            ;; AKTIVNÍ BOD NENÍ BLÍŽ: Podíváme se, jestli není vzdálenost náhodou stejná
            (if (equal? (calcDis target (list (cadr currPoint) (caddr currPoint))) 
                        (calcDis target (list (cadr acc) (caddr acc))))
                ;; SHODA VZDÁLENOSTÍ: Pokud mají dva různé body stejnou vzdálenost k targetu,
                ;; přepíšeme jméno v acc na tečku '#\.', což značí sporné území.
                (closestPoint target (cdr points) (list #\. (cadr acc) (caddr acc)))
                ;; AKTUÁLNÍ BOD JE DÁL: Ignorujeme ho a pokračujeme se starým acc
                (closestPoint target (cdr points) acc)
            )
        )
      )
  )
)

;; Funkce zkontroluje, zda se na souřadnicích 'target' nenachází přímo nějaký generující bod.
;; Pokud ano, vrátí jeho znak (např. #\A), jinak vrátí výchozí hodnotu #\-
(define (matchPoint target points [acc #\-])
  (if (empty? points)
      acc
      (let ([currPoint (car points)])
        ;; Pokud se souřadnice shodují, zapamatuje si znak bodu (car currPoint)
        (if (equal? target (list (cadr currPoint) (caddr currPoint)))
            (matchPoint target (cdr points) (car currPoint))
            (matchPoint target (cdr points) acc)
        )
)))

;; Hlavní funkce, která vygeneruje mřížku (grid) jako seznam řetězců
(define (grid points)
  ;; Určíme výšku (h) a šířku (w) mřížky podle nejvzdálenějšího bodu (+ 1 kvůli indexování od 0)
  (let ([h (+ (cadr (largestPoint points)) 1)]
        [w (+ (car (largestPoint points)) 1)])
    
    ;; Vnější cyklus přes Y souřadnice (řádky)
    (for/list ([y (in-range h)])
      ;; Převede vygenerovaný seznam znaků v řádku na výsledný String
      (list->string
       ;; Vnitřní cyklus přes X souřadnice (sloupce v řádku)
       (for/list ([x (in-range w)])
         ;; 1. Krok: Zjistíme, jestli na [x, y] nestojí přímo nějaký hlavní bod
         (if (equal? (matchPoint (list x y) points) #\-) 
             ;; Pokud tam hlavní bod nestojí, najdeme nejbližší bod, 
             ;; vezmeme jeho znak a převedeme ho na malé písmeno (char-downcase)
             (char-downcase (car (closestPoint (list x y) points)))
             ;; Pokud tam hlavní bod stojí, vykreslíme ho přímo jako velké písmeno
             (matchPoint (list x y) points)
         ))
      )
     )
   )
)

;; --- TESTOVACÍ DATA A VOLÁNÍ ---

(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))

;; Otestování, jaký znak je nejblíže bodu [3, 5]
(car (closestPoint '(3 5) points))

;; Vykreslení celé mřížky
(grid points)