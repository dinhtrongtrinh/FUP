#lang racket
(provide grid)

(define (maxPoint points [Max (car points)])
  (if (empty? points)
      (list (cadr Max) (caddr Max))
      (let* ([currPoint (car points)]
             [currX (cadr currPoint)]
             [currY (caddr currPoint)]
             [maxX (cadr Max)]
             [maxY (caddr Max)])
        (maxPoint (cdr points) (list #\A (max maxX currX) (max maxY currY)))
       )
  )
)
(define (calcDis fst snd)
  (let ([fstX (car fst)]
        [fstY (cadr fst)]
        [sndX (car snd)]
        [sndY (cadr snd)])
    (+ (abs (- fstX sndX)) (abs (- fstY sndY)))
  )
)

(define (closestPoint points curr [acc #\A] [minDis 999])
  (if (empty? points)
      acc
      (let* ([currPoint (car points)]
            [currChar (car currPoint)]
             [currX (caddr currPoint)]
             [currY (cadr currPoint)])

        (cond [(< (calcDis (list currX currY) curr) minDis)
               (closestPoint (cdr points) curr currChar (calcDis (list currX currY) curr))]
              [(equal? (calcDis (list currX currY) curr) minDis)
               (closestPoint (cdr points) curr #\. minDis)]
              [else
               (closestPoint (cdr points) curr acc minDis)]
        ) 

      )
  )
)
(define (matchPoint points curr)
  (if (empty? points) #f
  (let* ([currPoint (car points)]
            [currChar (car currPoint)]
             [currX (caddr currPoint)]
             [currY (cadr currPoint)])
    (if (equal? (list currX currY) curr)
        #t
        (matchPoint (cdr points) curr)
    )
  )
  )
)

(define (grid points)
    ; Implement me!
 (let* ([maxP (maxPoint points)]
        [W (car maxP)]
        [H (cadr maxP)])
   (for/list ([i (in-range (+ H 1))])
    (list->string (for/list ([j (in-range (+ W 1))])
        (if (matchPoint points (list i j))
            (closestPoint points (list i j))
            (char-downcase (closestPoint points (list i j)))
        )
                    
    ))
 )
 )
)
(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))

(define idk'("aaaaa.ccc"
  "aAaaa.ccc"
  "aaaddeccc"
  "aadddeccC"
  "..dDdeecc"
  "bb.deEeec"
  "bBb.eeee."
  "bbb.eeeff"
  "bbb.eefff"
  "bbb.ffffF") )

(closestPoint points '(0 0))
(matchPoint points '(1 1))
(equal? (grid points) idk)