#lang racket


(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (stream-mul (stream-rest s1) (stream-rest s2))))

(define (nums-from n)
    (stream-cons n (nums-from (+ 1 n)))
)

(define factorial-stream
    (stream* 1 (stream-mul factorial-stream (stream-rest (nums-from 1))))
)

    

(stream->list (stream-take factorial-stream 10))

