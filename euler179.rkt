
#lang racket

;; Specific code for Project Euler 179

(require "structures.rkt")
(require "compiler.rkt")
(require "command.rkt")
(require "numbers.rkt")
(require "code.rkt")
(require "flowcontrol.rkt")
(require "array.rkt")

(define var/i (var 'i 0))
(define var/j (var 'j 2))
(define var/p (var 'p 4))
(define var/valuation (var 'valuation 6))
(define var/tmp1 (var 'tmp1 8))
(define var/tmp2 (var 'tmp2 10))
(define var/constant (var 'constant 12)) ; Should be next to var/-1 for the padding to work out
(define var/-1 (var 'minus-one 14)) ; Must be the largest non-array variable

(define array-start-index (+ 2 (var-index var/-1)))

;; Assuming var/constant and var/-1 are two positions away from each
;; other, we find (based on the algorithm used in numbers.rkt) that if
;; we want to be able to generate any of the nonnegative integers up
;; to 10^n (inclusive), it will take at most 264 n + 338 characters to
;; do so (assuming we're pointing at the ops wheel and both wheels are
;; in noop position). We can rely on this by padding all of our
;; jump-relevant number generation to this length so that we can
;; calculate jump positions without needing to do fixed point
;; nonsense. That is, provided our program length is less than 10^n,
;; the jump length is independent of the code used to generate the
;; jump length value, which allows us to calculate it.
(define program-length-upper-bound-exp 5) ; 10^5 is the upper bound on our program length right now.
(define length-of-jump-instructions (+ 338 (* 264 program-length-upper-bound-exp)))

(define project-euler-179
  (build-whirl var/-1 length-of-jump-instructions
    (store-constant var/i 0)
    (assign var/valuation var/i)
    (->bool var/valuation)
    (while-nonzero var/valuation var/constant
      (seek-var var/i)
      (print-number)
      (store-constant var/constant 10)
      (print-ascii)
      (add var/i var/-1)
      (assign var/valuation var/i)
      (->bool var/valuation))
    (store-constant var/i 999)
    (print-number)
    (store-constant var/constant 10)
    (print-ascii)))

(define project-euler-179-test
  (build-whirl var/-1 length-of-jump-instructions
    (store-constant var/i 10)
    (print-number)
    (store-constant var/i 10)
    (print-ascii)
    (store-constant var/i 42)
    (store-constant var/j 3)
    (array-set array-start-index var/i var/j var/tmp1 var/tmp2)
    (store-constant var/i 10)
    (print-number)
    (store-constant var/i 10)
    (print-ascii)
    (store-constant var/i 3)
    (array-get array-start-index var/j var/i var/tmp1 var/tmp2)
    (print-number)
    (store-constant var/i 10)
    (print-ascii)
    (store-constant var/j 99)
    (print-number)
    (store-constant var/i 10)
    (print-ascii)))

(provide project-euler-179)
