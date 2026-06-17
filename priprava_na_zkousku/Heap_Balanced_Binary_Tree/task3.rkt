#lang racket

; 1. POVINNÉ NASTAVENÍ ZE ZADÁNÍ
(provide node node-v node-left node-right is-leaf? build-heap)

(struct node (v left right) #:transparent)

(define (is-leaf? nd)
  (eq? 'leaf nd))

; 2. POMOCNÁ FUNKCE PRO VYKRESLENÍ (Dali ti ji k dispozici pro testování)
(define (show-tree tree [depth 0])
  (define (offset d)
    (if (= d 0)
        ""
        (string-append "---" (offset (- d 1)))))
  (if (is-leaf? tree)
      tree
      (begin
        (show-tree (node-left tree) (+ depth 1))
        (displayln (string-append (offset depth) (number->string (node-v tree))))
        (show-tree (node-right tree) (+ depth 1))
        tree)))

;pokud leva je vetsi, tak true; else false
(define (min-depth root)
    (if (is-leaf? root)
        0
        (+ 1 (min (min-depth (node-left root))
                (min-depth (node-right root)))    
    ))
)
(define (insertAtEmpty val tree)
  (define (dfs stree)
    (cond [(is-leaf? stree) (node val 'leaf 'leaf)]
          [(< (min-depth (node-right stree)) (min-depth (node-left stree))) 
           (node (node-v stree) (node-left stree) (dfs (node-right stree)))]
          [#t 
           (node (node-v stree) (dfs (node-left stree)) (node-right stree))]))
  (cond [(is-leaf? tree) (node val 'leaf 'leaf)]
        [#t (dfs tree)]))


(define (enforceHeap tree)
  (cond [(is-leaf? tree) 'leaf]
        [#t 
         (let* ([left (enforceHeap (node-left tree))]
                [right (enforceHeap (node-right tree))]
                [v (node-v tree)])
           (cond [(and (not (is-leaf? left)) (> (node-v left) v) (or (is-leaf? right) (>= (node-v left) (node-v right))))
                  (node (node-v left) (node v (node-left left) (node-right left)) right)]
                 [(and (not (is-leaf? right)) (> (node-v right) v))
                  (node (node-v right) left (node v (node-left right) (node-right right)))]
                 [#t (node v left right)]))]))


(define (build-heap ls)
  (if (null? ls)
      'leaf
      (enforceHeap (insertAtEmpty (car ls) (build-heap (cdr ls))))
  ))


; --- TESTOVACÍ BLOK PRO TEBE ---
; Tyto řádky ti po spuštění v terminálu ukážou výsledek

;test stromu


(displayln "Test naší haldy:")
(show-tree (build-heap '(8 9 4 29)))