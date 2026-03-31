#lang racket

;;; Library for converting 2htdp/images into ascii-art strings
;;; Provides img->mat, ascii-art

(require 2htdp/image)
(provide/contract
 ; Ignore the contracts. They are here to catch errors.
 ; We will not discuss them further.
 [img->mat (image? . -> . (listof (listof number?)))]
 [ascii-art (fixnum? fixnum? string? . -> . (image? . -> . string?))])

(module+ test-support
  ; Use a submodule to export internal functions for unit testing.
  (provide color->gray index-formula))

; -----------------------------------------------------------------------------

;;; Converts an 8-bit RGB color into grayscale in [0,255].
(define (color->gray color)
  (+ (* 0.30 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color))))

;;; Takes a gray value in [0, 255] and maps it to an index in 0:len-1.
(define (index-formula len gray)
  (exact-floor (/ (* len (- 255 (floor gray))) 256)))

; -----------------------------------------------------------------------------
;                   START YOUR IMPLEMENTATION BELOW
; -----------------------------------------------------------------------------

(define (list-split lst widht [answer '()])
  (if (empty? lst)
    (reverse answer)
    (list-split (drop lst widht) widht (cons (take lst widht) answer))
    )
)

;;; Takes an image and converts it into a list of lists of grayscale values.
(define (img->mat image)
  (let ([widht (image-width image)]
        [color (image->color-list image)])
    (list-split (map color->gray color) widht)))

;;; First, takes a specification consisting of block-width, block-height and
;;; a character-set; then returns a new function which can convert any image
;;; into a 2D ascii-art string.
(define (ascii-art block-width block-height charset)
  (define (convert image)
    (let* ([matice (img->mat image)]
           [len (string-length charset)]
           [rows (length matice)]
           [cols (if (empty? matice) 0 (length (first matice)))]
           [valid-rows (* block-height (quotient rows block-height))]
           [valid-cols (* block-width (quotient cols block-width))]
           [cropped-mat (take matice valid-rows)]
           [cropped-mat (map (lambda (row) (take row valid-cols)) cropped-mat)]
           [bands (list-split cropped-mat block-height)]
           [process-band 
            (lambda (band)
              (let* ([chunked-rows (map (lambda (row) (list-split row block-width)) band)]
                     [blocks (apply map list chunked-rows)])
                (map (lambda (block)
                       (let* ([pixels (apply append block)]
                              [avg (/ (apply + pixels) (length pixels))])
                         (string-ref charset (index-formula len avg))))
                     blocks)))]
           [matice-znaku (map process-band bands)]
           [seznam-radku (map list->string matice-znaku)])
      (string-join seznam-radku "\n")))
  convert)

; Hint: If You decide to modify the list-split function from lab2,
; try pre-computing the number of splits, rather then checking the
; remaining list length on every iteration.