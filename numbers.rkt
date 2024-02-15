
#lang racket

;; Helper to generate numerical constants. This function should be run
;; inside a build-whirl block.

(require threading)

(require "code.rkt")
(require "command.rkt")
(require "compiler.rkt")
(require "utils.rkt")

(define/contract (put-constant n)
  ;; Puts the constant at the current memory position. Clobbers math
  ;; and operations ring values.
  (-> integer? code/c)
  (let ([comment (format "put constant ~a" (digits->words n))]
        [digits (number->digits (abs n))]
        [destination-pos (get-field memory-position (interpreter-state))])
    (code
     (exec math/set-to-zero)
     (exec math/store-memory)
     (cond
       ([= n 0] (code))
       ([> n 0] (put-constant>0 destination-pos digits))
       ([< n 0] (code (put-constant>0 destination-pos digits) (exec math/negate))))
     (exec math/store-memory)
     #:comment comment)))

(define (put-constant>0 destination-pos digits)
  ;; Assuming some leading digits are present at the indicated memory
  ;; position, generate the remainder and tack it onto the end.
  (let ([temporary-pos (+ 1 destination-pos)])
    (if (null? digits)
        (code) ; Done.
        (code
         (seek-memory temporary-pos)
         (make-small-constant 10)
         (seek-memory destination-pos)
         (exec math/*)
         (exec math/store-memory)
         (seek-memory temporary-pos)
         (make-small-constant (car digits))
         (seek-memory destination-pos)
         (exec math/+)
         (exec math/store-memory)
         (put-constant>0 destination-pos (cdr digits))))))

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

(provide put-constant)
