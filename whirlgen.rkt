
#lang racket

;; Generator script for programs in the Whirl programming language.

;; Conventions to make this actually remotely sane.
;;
;; (1) Assume all wheels are facing clockwise at all times. Anytime I
;; need to execute something, I'll double-rotate so that we're
;; clockwise after execution.
;;
;; (2) Whenever we do a jump, the math wheel should be on NOOP, and
;; the op wheel should be on IF. Anytime we end up at a jump target
;; (even if we just came from before that position), this should be
;; the case. That means that trivial jumps of length zero are fair
;; game.
;;
;; (3) As a corollary to the above, the P-ADD instruction is outright
;; banned. There is no safe way to do an unconditional jump and end up
;; in a consistent state. If I need to unconditionally jump, I'll do a
;; conditional jump with a constant condition.
;;
;; (4) Data is stored at the even positions (0-indexed) in the memory
;; buffer. ALL odd positions in the memory buffer are assumed to be
;; garbage temporaries. This is so that I can generate constant
;; numbers easily and move around.
;;
;; (5) Variables are stored at the low magnitude even positions. Our
;; big array is stored at the trailing right end of the memory buffer.
;; Again, all useful values are stored at even positions, so to move
;; to an array position we'll need to multiply the desired index by 2.
;;
;; (6) The largest variable that is NOT part of the trailing array is
;; the constant -1, which we'll use to move left in memory.

(require "structures.rkt")
(require "state.rkt")

(define var/-1 (var 'minus-one 2))

(let ([state (new state% [var/-1 var/-1])])
  (println (send state get-active-ring)))
