;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Zian Wang (20895169)
;; CS 135 Fall 2020
;; Assignment 07, Problem 1
;; ***************************************************
;;

;;
;; (a)
;;

;; (super-filter pred? nelst) apply the predicate to filter non-list elementfs
;; in each nested list
;; Examples:
(check-expect
 (super-filter
  odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? nelst)
  (cond [(empty? nelst) empty]
        [(list? (first nelst))
         (cons (super-filter pred? (first nelst))
               (super-filter pred? (rest nelst)))]
        [(pred? (first nelst))
         (cons (first nelst) (super-filter pred? (rest nelst)))]
        [else (super-filter pred? (rest nelst))]))

;; Tests:
(check-expect (super-filter even? empty) empty)  
(check-expect
 (super-filter
  even?
  (list 2 4 (list 4 (list 3 5 7 5 4 8) (list 1 3 4 2) (list 1 1 1)) (list 2 4)))
  (list 2 4 (list 4 (list 4 8) (list 4 2) '()) (list 2 4)))
(check-expect
 (super-filter
  even?
  (list 2 4 (list 4 (list 4 8) (list 4 2) '()) (list 2 4)))
  (list 2 4 (list 4 (list 4 8) (list 4 2) '()) (list 2 4)))

(define (string<5 x) (< (string-length x) 5))
(check-expect
 (super-filter
  string<5
  '("asldifjdasi" (("lisdf")) "sofdsjfsi"
                  ("slidfjsd" "sdf" "gfdg") ("sdfsd" "tret")))
 (list (list '()) (list "sdf" "gfdg") (list "tret")))



;;
;; (b)
;;

;; (ruthless lst): consumes a nested-list of symbol and remove symbol 'ruth
;; from the nested list
;; Examples:
(check-expect
 (ruthless
  (list 'rabbit
        (list 'apple 'pluto
              (list 'ruth 'blue) 'ruth) 'hello))
 (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless lst)
  (local [(define (notruth? sym)
            (not (symbol=? 'ruth sym)))]
    (super-filter notruth? lst)))

;; Tests:
(check-expect
 (ruthless
  empty) empty)
(check-expect
 (ruthless
  (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))
 (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))
(check-expect
 (ruthless '(ruth ruth (ruth ruth (ruth ruth)) ruth)) (list (list '())))



;;
;; (c)
;;

;; (superize n lst) consumes a Int and a nested-listof Int and remove
;; all numbers less than n from a nested list of natural numbers
;; Examples:
(check-expect
 (supersize 4 (list 8 1 (list 2 6 3) 10 1))
 (list 8 (list 6) 10))

;; superize: Num (nested-listof Int) -> (nested-listof Int)
(define (supersize n lst)
  (local [(define (>=n input) (>= input n))]
    (super-filter >=n lst)))

;; Tests:
(check-expect (supersize 10 empty) empty)
(check-expect
 (supersize 10 (list 8 1 (list 2 6 3) 10 1))
 (list '() 10))
(check-expect
 (supersize 1 (list 8 1 (list 2 6 3) 10 1))
 (list 8 1 (list 2 6 3) 10 1))
(check-expect
 (supersize 20 (list 8 1 (list 2 6 3) 10 1))
 (list (list )))



;;
;; (d)
;;

;; (super-keeper pred? lst) consumes a funtion and a nested list of x
;; produces a list with the elements of lst for which the predicate pred?
;; produces a false value
;; Examples:
(check-expect
 (super-keeper
  odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; super-keeper: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-keeper pred? lst)
  (local [(define (anti-keeper x) (not (pred? x)))]
    (super-filter anti-keeper lst)))

;; Tests:
(check-expect (super-keeper even? empty) empty)
(check-expect
 (super-keeper
  even?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))

(define (n<4? x) (< x 4))
(check-expect
 (super-keeper
  n<4?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list (list 4) 5 6 (list 7 8 9)) 10 11 12))

(define (string>=5 x) (>= (string-length x) 5))
(check-expect
 (super-keeper
  string>=5
  '("asldifjdasi" (("lisdf")) "sofdsjfsi"
                  ("slidfjsd" "sdf" "gfdg") ("sdfsd" "tret")))
 (list (list '()) (list "sdf" "gfdg") (list "tret")))
