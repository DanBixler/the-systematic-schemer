#lang racket

(require rackunit
         rackunit/text-ui ; Required for the run-tests reporter
         "../../core/atom.rkt")

;; member?

;; member? is one of:
;; - empty
;; - (cons Atom ListOfAtoms)
;; interp. the given atom either matches an atom in the list (#true) or not (#false)

;(define (lat? lat a) empty) ;stub

;; ---- Template ------
#;
(define (member? a lat)
  (cond [(empty? lat) (...)]
        [else
         (... (first lat) (a)
              (member? a (rest lat)))]))

;; member? -> Boolean
;; consumes an input, produces (true) if that input matches an atom in a list, (false) if it does not

;; --- Book version of member? ---

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #false)
      (else
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))))


;; --- more Racket-like version ---

#;(define (member? a lat)
  (cond
    [(empty? lat) #f]                ;; base case: if the list is empty, then there is no match
    [else
     (or (eq? (first lat) a)             ;; is the atom equal to the first item in the list
         (member? a (rest lat)))]))  ;; recursively go through the rest of the list to see if there is a match, if there is, it's true, if not we hit the empty list - false


;; ==========================================================
;; Tests


(module+ test
  (define member-tests
    (test-suite
     "member? tests"
     (check-true (member? 'meat '(potatoes meat gravy)))
     (check-false (member? 'meat '(potatoes chicken gravy))))) 
  (run-tests member-tests))