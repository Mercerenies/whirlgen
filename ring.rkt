
#lang racket

(require "command.rkt")

(define/contract ring%
  (class/c (init-field (name ring-name/c))
           (field (position integer?))
           (init-field (size integer?))
           [rotate-to (->m integer? string?)]
           [clone (->m (is-a?/c ring%))])
  (class object%
    (super-new)
    (init-field name)
    (field [position 0])
    (init-field size)
    (define/public (rotate-to new-position)
      (let ([distance (modulo (- new-position position) size)])
        (set! position new-position)
        (make-string distance #\1)))
    (define/public (clone)
      (let ([result (new ring% [name name] [size size])])
        (set-field! position result position)
        result))))

(define/contract ring/c
  contract?
  (is-a?/c ring%))

(provide ring% ring/c)
