#lang racket

(provide cheap-flight)

; Pomocné funkce pro práci s grafem ze zadání
(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))

; Pomocná funkce pro vyhledání sousedních hran uzlu
(define (get-neighbors start graf)
  (filter 
   (lambda (edge)
     (or (equal? (car edge) start) 
         (equal? (cadr edge) start)))
   (edges graf)))

; Hlavní funkce
(define (cheap-flight a b gr)
  
  ; Predikát pro řazení fronty od nejlevnějšího po nejdražší
  (define (cheaper? x y) (< (car x) (car y)))

  ; Rekurzivní vyhledávání s frontou
  (define (search queue)
    (cond
      [(null? queue) #f] ; Fronta je prázdná = cíl neexistuje, vrátíme #f
      [else
       (let* ([current (car queue)]       
              [cost (car current)]        
              [path (cadr current)]       
              [node (car path)])          
         
         (if (equal? node b)
             (list (reverse path) cost) ; JSME V CÍLI -> vracíme otočenou cestu a cenu
             
             ; NEJSME V CÍLI -> vygenerujeme nové cesty
             (let* ([all-extensions 
                    (map (lambda (e)
                           (let* ([next-node (if (equal? (car e) node) (cadr e) (car e))]
                                  [edge-cost (caddr e)]
                                  [new-cost (+ cost edge-cost)]
                                  [new-path (cons next-node path)])
                             (list new-cost new-path)))
                         (get-neighbors node gr))]
                    
                    ; OCHRANA PROTI ZACYKLENÍ: necháme jen ty cesty, kde 'next-node' ještě nebyl navštíven
                    [valid-extensions
                     (filter (lambda (elem)
                               (let ([next-node (car (cadr elem))])
                                 (not (member next-node path))))
                             all-extensions)])
               
               ; Spojíme zbytek fronty s novými platnými cestami, seřadíme a jedeme dál
               (search (sort (append (cdr queue) valid-extensions) cheaper?))
               )))]))

  ; Nastartování algoritmu s první cestou o ceně 0
  (search (list (list 0 (list a)))))