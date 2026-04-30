#lang racket

(require rackunit
         rackunit/text-ui
         "../../core/atom.rkt")


;; rember?


;; rember : Atom LAT -> LAT
;; Removes the first occurrence of atom 'a' from the list 'lat'.

;; -------- Data Definitions -------------------------
;;
;; A List-of-Atoms (LAT) is one of:
;; - '()
;; - (cons Atom LAT)
;; interp. a list where every element is an atom.


;; ------- stub and template -------------------------

;(define (rember x lat) '())    ;stub

#;
(define (rember a lat)
  (cond [(null? lat) (...)]
        [(eq? (car lat) a) (... (cdr lat))]
        [else
         (cons (car lat) 
               (rember a (cdr lat)))]))

;; --- Function --------------------------------------
;; from the book version...

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
         ((eq? (car lat) a)(cdr lat))
             (else (cons (car lat)
                   (rember a (cdr lat)))))))


;; more Racket-like version
#;
(define (rember x lat)
  (cond
    [(empty? lat) '()]                 ;; base case: return empty list if lat is empty
    [(eq? (first lat) x) (rest lat)]   ;; if first element matches x, remove it by returning the rest of the list
    [else (cons (first lat)
             (rember x (rest lat)))])) ;; otherwise reconstruct the list


;; ==========================================================
;; Tests

(module+ test
  (define rember-tests
    (test-suite "Tests for rember"
      (check-equal? (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
      (check-equal? (rember 'a '(a b c a d)) '(b c a d))
      (check-true (atom? 'mint)) #t)) ; Verifying our core import works

  (run-tests rember-tests))