#lang racket

(provide add-edge
         build-tree
         (struct-out node)
         (struct-out leaf))

(struct node (val kids) #:transparent)
(struct leaf (val) #:transparent)

(define (add-edge edge tree)
    (let

    ;pomocne promene 
        ()
        (cond [(leaf? tree)
                (if (equal? (leaf-val tree) (car edge))
                    (node (car edge) (list (leaf (cdr edge))))
                    tree
                )
            ]
            [(node? tree)
                (if (equal? (node-val tree) (car edge))
                    (node (car edge) (append (node-kids tree) (list (leaf (cadr edge)))))
                    (map (lambda (k) (add-edge k)) (node-kids tree))
                )
            
            ]
        )
    
    
    )
)

(define (build-tree init edges)
  ; Implement me!
    (cond [(null? edge) init]
        [#t (build-tree (add-edge (car edges) init) (cdr edges))])
)