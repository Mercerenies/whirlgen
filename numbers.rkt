
#lang racket

;; Helper to generate numerical constants.

(require threading)

(require "code.rkt")
(require "command.rkt")
(require "compiler.rkt")
(require "utils.rkt")
(require "structures.rkt")

(define/contract (put-constant n #:desired-length [desired-length #f])
  ;; Puts the constant at the current memory position. Clobbers math
  ;; and operations ring values. Should be run inside a build-whirl
  ;; block.
  (->* (integer?) (#:desired-length (or/c #f integer?)) code/c)
  (let ([comment (format "put constant ~a" (digits->words n))]
        [digits (number->digits (abs n) #:radix 2)])
    (code
     (exec math/set-to-zero)
     (exec math/store-memory)
     (cond
       ([= n 0] (code))
       ([> n 0] (code (put-constant>0 digits)))
       ([< n 0] (code (put-constant>0 digits) (exec math/negate))))
     (exec math/store-memory)
     #:comment comment
     #:desired-length desired-length)))

(define/contract (store-constant var n #:desired-length [desired-length #f])
  ;; Stores the constant at the given memory position. Clobbers math
  ;; and operations ring values. Should be run inside a build-whirl
  ;; block.
  (->* (var? integer?) (#:desired-length (or/c #f integer?)) code/c)
  (code
    (seek-var var)
    (put-constant n #:desired-length desired-length)))

(define (put-constant>0 digits)
  ;; Assuming some leading digits are present at the indicated memory
  ;; position, generate the remainder and tack it onto the end.
  (cond
    [(null? digits) (code)] ; Done
    [(= (car digits) 0) (code (mul2) (put-constant>0 (cdr digits)))]
    [(= (car digits) 1) (code (mul2) (add1) (put-constant>0 (cdr digits)))]))

(define (mul2)
  (code
   (exec math/load-memory)
   (exec math/+)
   (exec math/store-memory)))

(define (add1)
  (code
   (exec math/set-to-zero)
   (exec math/not)
   (exec math/+)
   (exec math/store-memory)))

(define (make-small-constant n)
  ;; Given a small positive number, produce that number on the math
  ;; wheel by unary construction (i.e. repeated adding of one). The
  ;; resulting string will have length linear in the magnitude of n,
  ;; which is why it's recommended to only use this function with
  ;; small numbers. Clobbers the current memory position.
  (case n
    [(0) (exec math/set-to-zero)]
    [(1) (code (exec math/set-to-zero) (exec math/not))]
    [else
     (apply code
            (exec math/set-to-zero)
            (exec math/not)
            (exec math/store-memory)
            (for/list ([i (in-range (- n 1))])
              (exec math/+)))]))

(define (digits->words n)
  (cond
    [(= n 0) "zero"]
    [(< n 0) (format "minus ~a" (digits->words (- n)))]
    [(> n 0) (~>> n
                  number->digits
                  (map digit->word)
                  (string-join _ " "))]))

(define/contract (calculate-longest-number var/destination var/-1 upper-limit)
  ;; Returns the length of the longest code required to build any
  ;; number from zero up to (and including) upper-limit, assuming the
  ;; interpreter is currently in no-op state and pointing at the
  ;; correct variable.
  (-> var? var? integer? integer?)
  (define (make-number n)
    (build-whirl var/-1 0 #:prelude #f ;; Length of jump instruction is not used, so set it to zero
      ;; Assume we're already at the desired memory position but do
      ;; NOT include that in the resulting code count.
      (seek-var var/destination)
      (put-constant n)))
  (for/fold ([max-length 0])
            ([i (in-range (+ 1 upper-limit))])
    (let ([code (make-number i)])
      (max max-length (send code length)))))

(provide put-constant store-constant
         calculate-longest-number)
