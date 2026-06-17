#lang racket/base
(require racket/string)
(require racket/list) ; Přidáno pro funkce jako 'empty?' a 'reverse'
(provide justify)

;; 1. POMOCNÁ FUNKCE: Vybere slova, která se vejdou na jeden řádek
;; (Vrátí je jako seznam řetězců, opraveno o bezpečné ukončení a otočení na konci)
(define (wordsInOneLine wordList maxWidht [listString '()] [acc 0])
  (cond
    ; Pokud už nemáme žádná slova, vrátíme to, co jsme nasbírali (otočené do správného pořadí)
    [(empty? wordList) (reverse listString)]
    
    ; Podmínka: Vejde se další slovo i s povinnou mezerou před ním?
    [else
     (let* ([current-word (car wordList)]
            [word-len (string-length current-word)]
            ; Pokud je to první slovo na řádku, nepotřebuje mezeru před sebou (acc = 0)
            [space-needed (if (= acc 0) word-len (+ 1 word-len))])
       
       (if (< maxWidht (+ acc space-needed))
           (reverse listString) ; Nevejde se -> řádek je plný, vrátíme ho otočený
           ; Vejde se -> rekurze se zbytkem slov, přidáme slovo na začátek a navýšíme acc
           (wordsInOneLine (cdr wordList) 
                           maxWidht 
                           (cons current-word listString) 
                           (+ acc space-needed))))]))

;; 2. POMOCNÁ FUNKCE: Vezme seznam slov pro jeden řádek a spravedlivě mezi ně rozdělí mezery
(define (oneLine listString maxWidth)
  (let* ([totalWordLen (apply + (map string-length listString))]
         [slots (- (length listString) 1)]
         [totalSpaces (- maxWidth totalWordLen)])
    
    (cond
      ; CHYTÁK 1: Řádek má jen jedno slovo -> slovo + všechny mezery za něj
      [(= slots 0) 
       (string-append (car listString) (make-string totalSpaces #\space))]
      
      ; HLAVNÍ PŘÍPAD: Více slov na řádku
      [else
       (let ([baseSpaces (quotient totalSpaces slots)] ; Celočíselné dělení (podíl)
             [remainder (modulo totalSpaces slots)]   ; Zbytek po dělení
             [slot-index 0]) 
         
         (string-join listString 
                      (lambda ()
                        (let ([space-to-make (if (< slot-index remainder) ; Opraveno porovnání indexu
                                                 (+ baseSpaces 1)
                                                 baseSpaces)])
                          (set! slot-index (+ 1 slot-index))
                          (make-string space-to-make #\space)))))])))

;; 3. HLAVNÍ FUNKCE: Propojuje vše dohromady do výsledného seznamu zarovnaných řádků
(define (justify wordList maxWidht)
  (if (empty? wordList)
      '() ; Pokud nemáme žádná slova, výsledek je prázdný seznam řádků
      (let* ([words-for-row (wordsInOneLine wordList maxWidht)] ; 1. Vezmeme slova pro první řádek
             [row-length (length words-for-row)]
             ; 2. Ořízneme původní seznam slov o ta slova, která jsme zrovna spotřebovali
             [remaining-words (drop wordList row-length)] 
             ; 3. Vyrobíme z nich jeden zarovnaný řádek (string)
             [formatted-row (oneLine words-for-row maxWidht)]) 
        
        ; 4. Spojíme tento hotový řádek s výsledkem rekurze pro zbytek textu
        (cons formatted-row (justify remaining-words maxWidht)))))