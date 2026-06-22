#lang racket
(provide spanning-tree (struct-out edge) graph)
(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)

(define (elem? x lst)
  (not (not (member x lst)))
)

(define (boundary-edge? edge covered)
  (if (and (elem? (edge-v edge) covered) (elem? (edge-u edge) covered))
      #f
      (if (or (elem? (edge-v edge) covered) (elem? (edge-u edge) covered))
          #t
          #f
      )
  )
)
(define (sortEdges edges)
  (sort edges
        (lambda (e1 e2) (< (edge-weight e1) (edge-weight e2))))
)
(define (find-next-edge edges covered)
  (let* ([edgeSort (sortEdges edges)]
        [currEdge (car edgeSort)])
    
    (if (boundary-edge? currEdge covered)
        currEdge
        (find-next-edge (cdr edgeSort) covered)
    )
  )
)
(define (whichVadd edge covered)
  (if (elem? (edge-v edge) covered)
      (edge-u edge)
      (edge-v edge)
  )
)

(define (spanning-tree gr [covered '()] [tree-edge '()])
  (cond [(= (length covered) (length (graph-nodes gr))) tree-edge]
        [(empty? covered)
         (let* ([ListEdges (graph-edges gr)]
                [currEdge (car ListEdges)]
                [U (edge-u currEdge)])
           (spanning-tree gr (list U) tree-edge) 
         )
        ]
        [else
         (let* ([edges (graph-edges gr)]
                [nextEdge (find-next-edge edges covered)]
                [addToC (whichVadd nextEdge covered)]
                [newCovered (cons addToC covered)]
                [newTreeEdge (cons nextEdge tree-edge)])
           (spanning-tree gr newCovered newTreeEdge)
         )
        ]
  )
)
;; Definice grafu z obrázku v zadání
(define gr (graph '(A B C D E F)
                  (list (edge 'A 'B 1)
                        (edge 'D 'E 4)
                        (edge 'E 'F 7)
                        (edge 'A 'D 5)
                        (edge 'B 'E 2)
                        (edge 'C 'F 5)
                        (edge 'D 'B 6)
                        (edge 'E 'C 4)
                        (edge 'A 'E 3))))

;; Spuštění tvého algoritmu
(define vysledek (spanning-tree gr))

;; --- TESTY ---

;; Test 1: Výsledný strom pro 6 vrcholů musí mít přesně 5 hran (V - 1)
(length vysledek) 
;; Mělo by vrátit: 5

;; Test 2: Celková váha minimální kostry musí být 16 (1 + 2 + 4 + 4 + 5)
(apply + (map edge-weight vysledek))
;; Mělo by vrátit: 16

;; Test 3: Vypiš si výsledek a zkontroluj, jestli obsahuje správné hrany
vysledek
;; Měly by tam být hrany s váhami 1, 2, 4, 4, 5 (pořadí hran ani vrcholů v nich nevadí)
  (define gr1 (graph '(A B C D E F)
                     (list (edge 'A 'B 1)
                           (edge 'D 'E 4)
                           (edge 'E 'F 7)
                           (edge 'A 'D 5)
                           (edge 'B 'E 2)
                           (edge 'C 'F 5)
                           (edge 'D 'B 6)
                           (edge 'E 'C 4)
                           (edge 'A 'E 3))))

> (spanning-tree gr1)








