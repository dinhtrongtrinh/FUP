#lang racket

;;; Example application for converting image bitmap files into ASCII art.
;;;
;;; Expects the arguments box-width, box-height and a file path.
;;; This application is a thin wrapper around the hw1 library.


(require 2htdp/image)
(require "./hw1.rkt")


;;; If a string is parsable as a positive-integer, bind it to id in a pattern.
(define-match-expander string->size?
  (syntax-rules () ((_ id) (app string->number (? positive-integer? id)))))


;;; Renders an 2htdp/image using the hw1.rkt module
(define (render-image w h img)
  (define charset " .,:;ox%#@")
  ((ascii-art w h charset) img))


;;; Tries to open a bitmap and render it to stdout as ascii-art
(define (try-render! w h path)
  (unless (file-exists? path)
    (printf "Could not open file ~a\n" path)
    (exit 1))
  (let* ((img (bitmap/file path))
         (art (render-image w h img)))
    (displayln art)))


;;; Tries to parse the ascii-art arguments
(define/match (main! _args)
  (((vector (string->size? width) path))
   (try-render! width width path))
  (((vector (string->size? width) (string->size? height) path))
   (try-render! width height path))
  ((_) (printf "Usage: racket ./example-app.rkt box-width [box-height] image-path\n")
       (exit 1)))


(main! (current-command-line-arguments))