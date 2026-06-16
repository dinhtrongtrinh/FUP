#lang racket

(provide accepts lwords (struct-out transition) (struct-out automaton))


(struct transition (from-state symbol to-state))
(struct automaton (trans init-state final-states))

;; Najde všechny dostupné stavy pro jeden konkrétní stav a jeden symbol
(define (findNextState currState trans name [acc '()])
  (if (empty? trans)
      acc
      (if (and (equal? (transition-from-state (car trans)) currState) 
               (equal? name (transition-symbol (car trans))))
          ;; Zde chyběly argumenty a volání (car trans) uvnitř transition-to-state
          (findNextState currState (cdr trans) name (cons (transition-to-state (car trans)) acc))
          (findNextState currState (cdr trans) name acc))))

;; Spočítá všechny dostupné stavy pro seznam aktuálních stavů a jeden symbol
(define (CTNS trans listCurr name [acc '()])
  (if (empty? listCurr)
      acc
      ;; Používáme append místo cons, aby se nevytvářely vnořené seznamy
      (CTNS trans (cdr listCurr) name (append (findNextState (car listCurr) trans name) acc))))

;; Zkontroluje průnik dvou seznamů (jestli je alespoň jeden prvek list1 v list2)
(define (checkList list1 list2)
  (if (empty? list1)
      #f
      (if (member (car list1) list2)
          #t
          (checkList (cdr list1) list2))))

;; Hlavní funkce pro ověření slova
(define (accepts automaton word)
  ;; Vytvoříme si vnitřní smyčku pro iteraci přes znaky
  (define (process-states current-states char-list)
    (if (empty? char-list)
        ;; Slovo je přečtené, zkontrolujeme, zda jsme v koncovém stavu (voláme struct accessor s automatem)
        (checkList current-states (automaton-final-states automaton))
        ;; Přečteme první znak a posuneme se do dalších stavů
        (process-states (CTNS (automaton-trans automaton) current-states (car char-list))
                        (cdr char-list))))
  
  ;; ZDE je klíčová úprava: string->list
  ;; Zároveň převedeme počáteční stav na seznam (list ...), protože CTNS očekává seznam stavů
  (process-states (list (automaton-init-state automaton)) 
                  (string->list word)))
(define (words alp n)
  (cond
    ((= n 0) '())
    ((= n 1) (map list alp))
    (else (apply append
                 (map (lambda (w)
                        (map (lambda (a) (cons a w)) alp))
                      (words alp (- n 1)))))))

(define (lwords alp nfa n)
  (filter (lambda (w) (accepts nfa (list->string w)))
          (words (string->list alp) n)))