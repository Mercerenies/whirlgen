
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
(define var/prime-power (var 'prime-power 8))
(define var/total (var 'total 10))
(define var/condition (var 'condition 12))
(define var/array-value (var 'array-value 14))
(define var/tmp1 (var 'temporary-one 16))
(define var/tmp2 (var 'temporary-two 18))
(define var/limit (var 'limit 20)) ; Will store the constant 10000001
(define var/constant (var 'constant 22)) ; Should be next to var/-1 for the padding to work out
(define var/-1 (var 'minus-one 24)) ; Must be the largest non-array variable

(define array-start-index (+ 2 (var-index var/-1)))

;; We pad all jump instructions to the same length. I originally
;; calculated a nice upper bound for this mathematically, but it ends
;; up being a pretty loose upper bound (especially with the
;; improvements that have since been made to the number production
;; algorithm), so now this number is arbitrarily set to be relatively
;; close to the actual required amount and adjusted as needed.
(define length-of-jump-instructions 900)

(define (setup-limit-constant)
  (store-constant var/limit 10000001))

(define (print-newline)
  (code (store-constant var/constant 10)
        (print-ascii)))

(define (debug-print var)
  (code (seek-var var) (print-number) (print-newline)))

(define (debug-print-mod var value)
  (code (assign var/tmp1 var)
        (store-constant var/constant value)
        (do-modulo var/tmp1 var/constant var/tmp2)
        (->bool var/tmp1)
        (if0 var/tmp1 var/tmp2
          (debug-print var))))

(define project-euler-179
  (build-whirl var/-1 length-of-jump-instructions

    ;; Initialization
    (setup-limit-constant)

    ;; Set largest_prime array values for i = 2 specifically.
    (store-constant var/i 2)
    (store-constant var/j 2)
    (do-while-nonzero var/condition var/tmp1
      (array-get array-start-index var/array-value var/j var/tmp1 var/tmp2)
      (array-set array-start-index var/i var/j var/tmp1 var/tmp2)
      (increment var/j)
      (increment var/j)
      (assign var/condition var/j)
      (cmp< var/condition var/limit))

    ;; Set largest_prime array values.
    (store-constant var/i 3)
    (do-while-nonzero var/condition var/tmp1
      (array-get array-start-index var/array-value var/i var/tmp1 var/tmp2)
      (->bool var/array-value)
      (if0 var/array-value var/tmp1
        ;; Update all multiples of the prime i.
        (assign var/j var/i)
        (do-while-nonzero var/condition var/tmp1
          (array-set array-start-index var/i var/j var/tmp1 var/tmp2)
          (add var/j var/i)
          (assign var/condition var/j)
          (cmp< var/condition var/limit)))
      (increment var/i)
      (increment var/i)
      (assign var/condition var/i)
      (cmp< var/condition var/limit))

    ;; Set divisors array values.
    (store-constant var/constant 1)
    (store-constant var/i 1)
    (array-set array-start-index var/constant var/i var/tmp1 var/tmp2)
    (store-constant var/i 2)
    (do-while-nonzero var/condition var/tmp1
      (array-get array-start-index var/p var/i var/tmp1 var/tmp2)
      (store-constant var/valuation 1)
      (assign var/prime-power var/p)
      (assign var/condition var/i)
      (assign var/tmp1 var/prime-power)
      (mul var/tmp1 var/p)
      (do-modulo var/condition var/tmp1 var/tmp2)
      (logical-not var/condition)
      (while-nonzero var/condition var/tmp1
        (mul var/prime-power var/p)
        (increment var/valuation)
        (assign var/condition var/i)
        (assign var/tmp1 var/prime-power)
        (mul var/tmp1 var/p)
        (do-modulo var/condition var/tmp1 var/tmp2)
        (logical-not var/condition))
      (assign var/j var/prime-power)
      (reversed-div var/j var/i)
      (array-get array-start-index var/array-value var/j var/tmp1 var/tmp2)
      (assign var/tmp1 var/valuation)
      (increment var/tmp1)
      (mul var/array-value var/tmp1)
      (array-set array-start-index var/array-value var/i var/tmp1 var/tmp2)
      (increment var/i)
      (assign var/condition var/i)
      (cmp< var/condition var/limit))

    ;; Iterate and count.
    (store-constant var/total 0)
    (add var/limit var/-1)
    (store-constant var/i 1)
    (do-while-nonzero var/condition var/tmp1
      (array-get array-start-index var/j var/i var/tmp1 var/tmp2)
      (increment var/i)
      (array-get array-start-index var/array-value var/i var/tmp1 var/tmp2)
      (cmp= var/array-value var/j)
      (if-nonzero var/array-value var/tmp1
        (increment var/total))
      (assign var/condition var/i)
      (cmp< var/condition var/limit))

    ;; Print result.
    (seek-var var/total)
    (print-number)
    (print-newline)))

(provide project-euler-179)
