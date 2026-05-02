#lang racket

(require rackunit
         rackunit/text-ui)


;; firsts

;; firsts : LOL -> LAT
;; Takes a list of non-empty lists and builds a list of the first elements.


;; -------- Data Definitions -------------------------
;;
;; A List-of-Lists (LOL) is one of:
;; - '()
;; - (cons (cons Atom LAT) LOL)
;; interp. a list where each element is itself a non-empty list of atoms.


;; ------- stub and template -------------------------

;(define (firsts x lat) '())    ;stub


#;
(define firsts
  (lambda (l)
    (cond
      ((null? l)...)
      (else (cons (car (car l))
                  (firsts(cdr l)))))))

;; --- Function --------------------------------------
(define (firsts l)
  (cond
    [(null? l) '()]
    [else
     (cons (car (car l))        ; Reach into the first inner list
           (firsts (cdr l)))])) ; Recurse on the rest of the outer list

;; ==========================================================
;; Tests

(module+ test
  (define firsts-tests
    (test-suite "Tests for firsts"
      (check-equal? (firsts '((apple pie) (banana split) (cherry tart))) 
                    '(apple banana cherry)
                    "Common case: 3 lists")
      (check-equal? (firsts '((a b) (c d) (e f))) 
                    '(a c e))
      (check-equal? (firsts '()) 
                    '()
                    "Base case: empty list")
      (check-equal? (firsts '((five plums) (four) (eleven tomatoes))) 
                    '(five four eleven)
                    "Works even if inner lists have only one element")))

  (run-tests firsts-tests))


;; ==========================================================
;; Tests