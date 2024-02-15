
#lang racket

(require "state.rkt")
(require "code.rkt")
(require "command.rkt")
(require "structures.rkt")

;; Compiler and builders for the Whirl generator.

(define interpreter-state (make-parameter #f))
(define jump-constant-length (make-parameter #f))

(define-syntax build-whirl
  (syntax-rules ()
    [(build-whirl var/-1 jump-length #:prelude use-prelude . body)
     (parameterize ([interpreter-state (new state% [var/-1 var/-1])]
                    [jump-constant-length jump-length])
       (code (if use-prelude (prelude) (code)) . body))]
    [(build-whirl var/-1 jump-length . body)
     (build-whirl var/-1 jump-length #:prelude #t . body)]))

(define-syntax-rule (local-whirl . body)
  ;; Runs code locally in an interpreter state cloned from the current
  ;; one.
  (parameterize ([interpreter-state (send (interpreter-state) clone)]) . body))

(define/contract (prelude)
  (-> code/c)
  (let ([var/-1 (get-field var/-1 (interpreter-state))])
    (code
      (send (interpreter-state) move-memory (var-index var/-1))
      (exec math/set-to-zero)
      (exec math/not)
      (exec math/negate)
      (exec math/store-memory)
      #:comment "Initialize negative one constant")))

(define/contract (exec command)
  ;; Low-level exec command that just moves the wheel(s) and executes.
  (-> command? code/c)
  (code (send (interpreter-state) execute command)))

(define/contract (print-number)
  ;; Prints the value at the current memory position, as an integer.
  (-> code/c)
  (code
    (exec op/set-to-one)
    (exec op/integer-io)
    #:comment "Print integer to stdout"))

(define/contract (print-ascii)
  ;; Prints the value at the current memory position, as ASCII.
  (-> code/c)
  (code
    (exec op/set-to-one)
    (exec op/ascii-io)
    #:comment "Print ASCII to stdout"))

(define/contract (seek-memory target-memory)
  ;; Moves the memory pointer to the given index. Clobbers operator
  ;; wheel's value.
  (-> integer? code/c)
  (code (send (interpreter-state) move-memory target-memory)))

(define/contract (seek-var target-var)
  ;; Moves the memory pointer to the given variable. Clobbers operator
  ;; wheel's value.
  (-> var? code/c)
  (code
    (seek-memory (var-index target-var))
    #:comment (format "Seek to ~a" (var-name target-var))))

(define/contract (assign destination-var source-var)
  ;; Sets the destination variable to the current value of the source
  ;; variable. Clobbers both wheel values and seeks to
  ;; destination-var.
  (-> var? var? code/c)
  (code
   (send (interpreter-state) move-memory (var-index source-var))
   (exec math/load-memory)
   (send (interpreter-state) move-memory (var-index destination-var))
   (exec math/store-memory)
   #:comment (format "Assign [~a] = [~a]" (var-name destination-var) (var-name source-var))))

(define/contract (add destination-var source-var)
  ;; Adds the source variable to the destination variable, storing the
  ;; result in the destination variable. Clobbers both wheel values
  ;; and seeks to destination-var.
  (-> var? var? code/c)
  (code
   (send (interpreter-state) move-memory (var-index source-var))
   (exec math/load-memory)
   (send (interpreter-state) move-memory (var-index destination-var))
   (exec math/+)
   (exec math/store-memory)
   #:comment (format "[~a] += [~a]" (var-name destination-var) (var-name source-var))))

(define/contract (mul destination-var source-var)
  ;; Multiplies the source variable to the destination variable,
  ;; storing the result in the destination variable. Clobbers both
  ;; wheel values and seeks to destination-var.
  (-> var? var? code/c)
  (code
   (send (interpreter-state) move-memory (var-index source-var))
   (exec math/load-memory)
   (send (interpreter-state) move-memory (var-index destination-var))
   (exec math/*)
   (exec math/store-memory)
   #:comment (format "[~a] *= [~a]" (var-name destination-var) (var-name source-var))))

(define/contract (logical-not var)
  ;; Logically invert the value at the given variable. Clobbers both
  ;; wheel values and seeks to var.
  (-> var? code/c)
  (code
   (send (interpreter-state) move-memory (var-index var))
   (exec math/load-memory)
   (exec math/not)
   (exec math/store-memory)
   #:comment (format "[~a] = NOT [~a]" (var-name var) (var-name var))))

(define/contract (->bool var)
  ;; Normalize to a zero or a one. Clobbers both wheels and seeks to var.
  (-> var? code/c)
  (code
   (send (interpreter-state) move-memory (var-index var))
   (exec math/load-memory)
   (exec math/not)
   (exec math/not)
   (exec math/store-memory)
   #:comment (format "Boolify [~a]" (var-name var))))

(provide build-whirl local-whirl
         prelude exec
         print-number print-ascii
         seek-memory seek-var
         assign add mul
         logical-not ->bool
         (contract-out
          (interpreter-state (-> (is-a?/c state%)))
          (jump-constant-length (-> integer?))))
