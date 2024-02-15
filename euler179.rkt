
#lang racket

;; Specific code for Project Euler 179

(require "structures.rkt")
(require "compiler.rkt")

(define var/i (var 'i 0))
(define var/j (var 'j 2))
(define var/p (var 'p 4))
(define var/valuation (var 'valuation 6))
(define var/-1 (var 'minus-one 8)) ; Must be the largest non-array variable

(define project-euler-179
  (build-whirl var/-1
    "A"))

(provide project-euler-179)
