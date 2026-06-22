#lang racket

(provide cheap-flight)
; list of nodes
(define ns '(1 2 3 4 5 6)) ; listofnodes
; list of edges where each edge contains (start end cost)
(define es '((1 2 0.5) (1 3 1.0) (2 3 2.0) (2 5 1.0) (3 4 4.0) (4 5 1.0)))
; the graph; a list of nodes and edges
(define gr (list ns es))

; some convenience functions
(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))

(define (cheapEdge routeList covered edges [acc '()])
  (if (empty? edges)
      acc ;; Když dojdou hrany, vrátíme nashromážděné nové trasy
      (let* ([currEdge (car edges)]
             [v (car currEdge)]
             [u (cadr currEdge)]
             [cost (caddr currEdge)]
             ;; Vytáhneme si z routeList aktuální stav: (cesta cena)
             [current-path (car routeList)]
             [current-cost (cadr routeList)]
             ;; Aktuální letiště, kde se nacházíme, je na vrcholu cesty
             [currAirport (car current-path)])
        
        (cond 
          ;; Případ A: Hrana vede z našeho letiště (v) do nového (u)
          [(and (equal? v currAirport) (not (member u covered)))
           (let ([new-route (list (cons u current-path) (+ current-cost cost))])
             (cheapEdge routeList covered (cdr edges) (cons new-route acc)))]
          
          ;; Případ B: Graf je neorientovaný, takže hrana může vést i z (u) do nového (v)
          [(and (equal? u currAirport) (not (member v covered)))
           (let ([new-route (list (cons v current-path) (+ current-cost cost))])
             (cheapEdge routeList covered (cdr edges) (cons new-route acc)))]
          
          ;; Případ C: Hrana s naším letištěm nesouvisí nebo vede do už navštíveného místa
          [else 
           (cheapEdge routeList covered (cdr edges) acc)]))))

(define (cheaper? x y) 
  (< (cadr x) (cadr y)))
(define (cheaper-edge? x y)
  (< (caddr x) (caddr y))
  )

(define (cheap-flight start end gr)
  (define (loop queue)
    (cond 
      ;; Pokud je fronta prázdná, cesta neexistuje
      [(empty? queue) #f]
      [else
       (let* ([sortQ (sort queue cheaper?)]
              [firstQ (car sortQ)]         ; Globálně nejlevnější plán
              [zbytekQ (cdr sortQ)]        ; Zbytek fronty
              [currPath (car firstQ)]       ; Jeho cesta (zároveň slouží jako covered!)
              [currCost (cadr firstQ)]      ; Jeho cena
              [currAirport (car currPath)]) ; Kde to letadlo zrovna stojí
         
         (cond 
           ;; KONTROLA CÍLE: Kontrolujeme to nejlepší z fronty!
           [(equal? currAirport end) 
            (list (reverse currPath) currCost)]
           
           ;; EXPANZE: Pokud nejsme v cíli, vygenerujeme nové plány a hodíme je do fronty
           [else 
            (let* ([grEdges (edges gr)]
                   ;; Jako covered posíláme currPath (historii téhle cesty)
                   [nove-cesty (cheapEdge firstQ currPath grEdges)])
              ;; Všechny nové cesty spojíme se zbytkem fronty a jedeme další kolo
              (loop (append nove-cesty zbytekQ)))]))]))

  ;; Startovní zavolání: ve frontě je jedna cesta se startovním letištěm a cenou 0.0
  (loop (list (list (list start) 0.0))))


; with the graph defined above:
> (cheap-flight 2 4 gr)
'((2 1 3) 1.5)

> (cheap-flight 2 2 gr)
#f