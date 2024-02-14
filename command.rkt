
#lang racket

(struct command (ring-name index) #:transparent)

(define ring-name/c
  (or/c 'operations 'math))

(define noop 0)
(define ring-size 12)

(define op/noop (command 'operations noop))
(define op/exit (command 'operations 1))
(define op/set-to-one (command 'operations 2))
(define op/set-to-zero (command 'operations 3))
(define op/load-memory (command 'operations 4))
(define op/store-memory (command 'operations 5))
(define op/add-program (command 'operations 6))
(define op/add-memory (command 'operations 7))
(define op/logical-and (command 'operations 8))
(define op/add-program-if (command 'operations 9))
(define op/integer-io (command 'operations 10))
(define op/ascii-io (command 'operations 11))

(define math/noop (command 'math noop))
(define math/load-memory (command 'math 1))
(define math/store-memory (command 'math 2))
(define math/+ (command 'math 3))
(define math/* (command 'math 4))
(define math/div (command 'math 5))
(define math/set-to-zero (command 'math 6))
(define math/< (command 'math 7))
(define math/= (command 'math 8))
(define math/> (command 'math 9))
(define math/not (command 'math 10))
(define math/negate (command 'math 11))

(provide
 (contract-out
  [struct command
    ((ring-name ring-name/c) (index integer?))]
  [ring-name/c contract?]
  [noop integer?]
  [ring-size integer?]
  [op/noop command?]
  [op/exit command?]
  [op/set-to-one command?]
  [op/set-to-zero command?]
  [op/load-memory command?]
  [op/store-memory command?]
  [op/add-program command?]
  [op/add-memory command?]
  [op/logical-and command?]
  [op/add-program-if command?]
  [op/integer-io command?]
  [op/ascii-io command?]
  [math/noop command?]
  [math/load-memory command?]
  [math/store-memory command?]
  [math/+ command?]
  [math/* command?]
  [math/div command?]
  [math/set-to-zero command?]
  [math/< command?]
  [math/= command?]
  [math/> command?]
  [math/not command?]
  [math/negate command?]))
