#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "../../core/atom.rkt")

;; lat - list of atoms

;; ==========================================================
;; Data Definitions

;; A List-of-Atoms (LAT) is one of:
;;  - '()
;;  - (cons Atom LAT)
;; interp. a list where every element is a single, indivisible value

;; ==========================================================
;; Functions

;; lat? : LAT -> Boolean
;; produce #t if every element in the list is an atom; #f otherwise


#|---- Template ------
 (define (fn-for-lat l)
   (cond [(null? l) (...)]                ; Base Case: The list is empty
         [else
          (... (car l)                    ; The first element (an Atom)
               (fn-for-lat (cdr l)))]))   ; Natural Recursion on the rest

 LAT -> Boolean
 produce #t if every element in the list is an atom; #f otherwise
|#


(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]        ; If car is an atom, check the rest
        [else #f]))                             ; If car isn't an atom, it's not a LAT


;; --- Tests (using Rackunit for #lang racket) ---

(module+ test
  (define lat-tests
    (test-suite
     "Testing lat?"
    (check-true  (lat? '(bacon lettuce tomato)))
    (check-false (lat? '(bacon (lettuce) tomato)))
    (check-true  (lat? '()))))

  (run-tests lat-tests))