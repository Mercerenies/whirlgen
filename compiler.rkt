
#lang racket

(require "state.rkt")
(require "code.rkt")

;; Compiler and builders for the Whirl generator.

(define interpreter-state (make-parameter #f))

(define-syntax build-whirl
  (syntax-rules ()
    [(build-whirl var/-1 #:prelude use-prelude . body)
     (parameterize ([interpreter-state (new state% [var/-1 var/-1])])
       (code (if use-prelude (prelude) (code)) . body))]
    [(build-whirl var/-1 . body)
     (build-whirl var/-1 #:prelude #t . body)]))

(define/contract (prelude)
  (-> code/c)
  (code))

(provide build-whirl prelude)
