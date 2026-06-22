#lang racket

(struct node (v left right) #:transparent)

(define (is-leaf? nd)
  (eq? 'leaf nd))

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
       tree)
      ))

(define (insertAtEmpty(x


