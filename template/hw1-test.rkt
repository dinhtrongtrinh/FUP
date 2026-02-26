#lang racket

(require rackunit rackunit/text-ui)
(require 2htdp/image)
(require "./hw1.rkt" (submod "./hw1.rkt" test-support))

(define charset " .,:;ox%#@")
(define charset-hex "0123456789ABCDEF")
(define charset-bw " #")

(define image-black-pixel (rectangle 1 1 "solid" "black"))
(define image-white-pixel (rectangle 1 1 "solid" "white"))
(define image-gray-rectangles
  (above
   (beside (rectangle 2 1 "solid" (make-color 0 0 0))
           (rectangle 3 1 "solid" (make-color 75 75 75)))
   (beside (rectangle 2 3 "solid" (make-color 180 180 180))
           (rectangle 3 3 "solid" (make-color 225 225 225)))))

; -----------------------------------------------------------------------------

(define color->gray-suite
  (test-suite
   "color->gray unit tests"

   (test-case
    "color->gray knows that black is dark"
    (check-= (color->gray (color 0 0 0)) 0 1e-3))

   (test-case
    "color->gray knows that white is bright"
    (check-= (color->gray (color 255 255 255)) 255 1e-3))

   (test-case
    "color->gray returns approximate lightness of bright red"
    (check-= (color->gray (color 255 0 0)) (* 0.3 255) 1e-1))))

; -----------------------------------------------------------------------------

(define index-formula-suite
  (test-suite
   "index-formula unit tests"

   (test-case
    "index-formula matches black to last"
    (check-equal? (index-formula 3 0) 2))

   (test-case
    "index-formula matches white to zero"
    (check-equal? (index-formula 3 255) 0))

   (test-case
    "index-formula matches reverse gradient"
    (let* ((grays (inclusive-range 0 255))
           (expected (reverse grays))
           (actual (map (curry index-formula 256) grays)))
      (check-equal? actual expected)))

   (test-case
    "index-formula is uniform on a fencepost gradient"
    (let* ((grays (inclusive-range 0 255 255/15))
           (expected (inclusive-range 15 0 -1))
           (actual (map (curry index-formula 16) grays)))
      (check-within actual expected 1e-3)))))

; -----------------------------------------------------------------------------

(define img->mat-suite
  (test-suite
   "img->mat unit tests"

   (test-case
    "black pixel is a zero matrix"
    (let* ((image image-black-pixel)
           (expected '((0)))
           (actual (img->mat image)))
      (check-within actual expected 1e-3)))

   (test-case
    "white pixel is a 255 matrix"
    (let* ((image image-white-pixel)
           (expected '((255)))
           (actual (img->mat image)))
      (check-within actual expected 1e-3)))

   (test-case
    "gray rectangles produce correct values"
    (let* ((image image-gray-rectangles)
           (expected '((0 0 75 75 75)
                       (180 180 225 225 225)
                       (180 180 225 225 225)
                       (180 180 225 225 225)))
           (actual (img->mat image)))
      (check-within actual expected 1e-3)))))

; -----------------------------------------------------------------------------

(define ascii-art-suite
  (test-suite
   "ascii-art end-to-end tests"

   (test-case
    "black pixel rendered as hash"
    (let* ((image image-black-pixel)
           (expected "#")
           (actual ((ascii-art 1 1 charset-bw) image)))
      (check-equal? actual expected)))

   (test-case
    "white pixel rendered as space"
    (let* ((image image-white-pixel)
           (expected " ")
           (actual ((ascii-art 1 1 charset-bw) image)))
      (check-equal? actual expected)))

   (test-case
    "gray rectangles mapped directly produce correct art"
    (let* ((image image-gray-rectangles)
           (expected "@@%%%\n,,...\n,,...\n,,...")
           (actual ((ascii-art 1 1 charset) image)))
      (check-equal? actual expected)))

   (test-case
    "gray rectangles average value is correct"
    (let* ((image image-gray-rectangles)
           (expected ":")
           (actual ((ascii-art 5 4 charset) image)))
      (check-equal? actual expected)))))

; -----------------------------------------------------------------------------

(run-tests
 (test-suite
  "all tests"
  index-formula-suite
  color->gray-suite
  img->mat-suite
  ascii-art-suite))
