
#lang racket

(require "code.rkt")
(require "command.rkt")
(require "compiler.rkt")
(require "utils.rkt")
(require "structures.rkt")
(require "numbers.rkt")

(define-syntax-rule (if0 var/condition var/temporary . body)
  ;; The value in var/condition MUST be a zero or a one at this point.
  ;; The body will be executed if and only if var/condition is zero.
  ;; At the end of the condition, the values of var/condition and
  ;; var/temporary are unspecified, both wheels are clobbered, and our
  ;; memory pointer is at var/condition.
  (let-values ([(precursor if-body) (local-whirl (compile-if0 var/condition var/temporary 0 . body))])
    (let ([actual-distance (+ (send if-body length) 1)])
      (let-values ([(precursor if-body) (compile-if0 var/condition var/temporary actual-distance . body)])
        (code precursor if-body)))))

(define-syntax-rule (compile-if0 var/condition var/temporary distance . body)
  (values
   ;; Outside of if statement
   (if0-initial-jump var/condition var/temporary distance)
   ;; Inside of if statement
   (code (code . body)
         (if0-end var/condition))))

(define (if0-initial-jump var/condition var/temporary distance)
  (code
   (store-constant var/temporary distance #:desired-length (jump-constant-length))
   (mul var/condition var/temporary)
   (code (exec op/load-memory) #:comment "Load condition variable")
   (spin-rings-to-noop)
   (code (exec op/add-program-if) #:comment "Conditional jump (If)")))

(define (if0-end var/condition)
  (code
   (seek-var var/condition)
   (code (exec op/set-to-zero)
         (exec op/store-memory)
         #:comment "Zero out condition variable")
   (spin-rings-to-noop)
   (code (exec op/add-program-if) #:comment "Trivial jump (Endif)")))

(define (spin-rings-to-noop)
  ;; Moves both rings to the noop position and guarantees that we are
  ;; pointing to the operations ring now.
  (code
    (exec op/noop)
    (exec math/noop)
    #:comment "Reset rings to noop"))

(provide if0)
