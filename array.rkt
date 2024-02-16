
#lang racket

(require "code.rkt")
(require "command.rkt")
(require "compiler.rkt")
(require "utils.rkt")
(require "structures.rkt")
(require "numbers.rkt")

;; Helpers for accessing the trailing end of the memory buffer as an
;; array.

(define/contract (array-get array-start-index var/destination var/index var/tmp1 var/tmp2)
  ;; Gets the value from the array and stores it in var/destination.
  ;; The memory pointer will be pointing at var/destination after
  ;; this.
  (-> integer? var? var? var? var? code/c)
  (code
   (array-op-precursor array-start-index var/destination var/index var/tmp1 var/tmp2)
   (code
    (seek-memory (+ 1 (var-index var/destination)))
    (exec op/load-memory)
    (exec op/add-memory)
    (exec op/store-memory)
    (exec math/load-memory)
    (exec math/negate)
    (exec math/store-memory)
    (exec op/set-to-one)
    (shift 2)
    (bootstrap-<-1>)
    (exec op/load-memory)
    (shift 3)
    (exec math/load-memory)
    (exec op/set-to-one)
    (shift 1)
    (exec op/load-memory)
    (exec op/add-memory)
    (seek-memory (var-index var/destination))
    (exec math/store-memory)
    #:comment (format "Get array value into ~a" (var-name var/destination)))))

(define/contract (array-set array-start-index var/source var/index var/tmp1 var/tmp2)
  ;; Sets the value from the array to the value of var/source. The
  ;; memory pointer will be pointing at var/source after this, whose
  ;; value is preserved.
  (-> integer? var? var? var? var? code/c)
  (code
   (array-op-precursor array-start-index var/source var/index var/tmp1 var/tmp2)
   (code
    (exec math/load-memory)
    (seek-memory (+ 1 (var-index var/source)))
    (exec op/load-memory)
    (exec op/add-memory)
    (exec op/store-memory)
    (exec op/set-to-one)
    (shift 2)
    (exec math/store-memory)
    (shift 2)
    (bootstrap-<-1>)
    (exec op/load-memory)
    (shift 2)
    (exec math/load-memory)
    (shift 3)
    (exec math/store-memory)
    (exec op/set-to-one)
    (shift 1)
    (exec math/load-memory)
    (exec math/negate)
    (exec math/store-memory)
    (exec op/load-memory)
    (exec op/add-memory)
    (seek-memory (var-index var/source))
    #:comment (format "Set array value to [~a]" (var-name var/source)))))

(define (array-op-precursor array-start-index var/source var/index var/tmp1 var/tmp2)
  ;; Sets up the initial jump for an array operation. After calling
  ;; this function, the memory buffer will be pointing at var/source.
  ;; The odd position var/(source + 1) contains the delta value to get
  ;; out to one past the target position in the array from var/(source
  ;; + 1).
  (let ([distance-to-array (- array-start-index (var-index var/source))])
    (code
      (assign var/tmp1 var/index)
      (exec math/load-memory)
      (exec math/+)
      (exec math/store-memory)
      (store-constant var/tmp2 distance-to-array)
      (add var/tmp1 var/tmp2)
      (code (exec math/load-memory)
            (seek-memory (+ 1 (var-index var/source)))
            (exec math/store-memory)
            (seek-memory (var-index var/source))
            #:comment "Prepare for array jump"))))

(define (bootstrap-<-1>)
  ;; Generate -1 at the current position, at the cost of clobbering
  ;; the math ring's value.
  (code
   (exec math/set-to-zero)
   (exec math/not)
   (exec math/negate)
   (exec math/store-memory)))

(define (shift n)
  ;; Add memory N times, using the current value of the op wheel.
  (apply code (for/list ([i (in-range n)]) (exec op/add-memory))))

(provide array-get array-set)
