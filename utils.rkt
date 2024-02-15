
#lang racket

(define/contract (number->digits n)
  (-> natural-number/c list?)
  (number->digits-rec n '()))

(define (number->digits-rec n acc)
  (if (zero? n)
      acc
      (number->digits-rec (quotient n 10) (cons (modulo n 10) acc))))

(define/contract (digit->word n)
  (-> (integer-in 0 9) string?)
  (case n
    ((0) "zero")
    ((1) "one")
    ((2) "two")
    ((3) "three")
    ((4) "four")
    ((5) "five")
    ((6) "six")
    ((7) "seven")
    ((8) "eight")
    ((9) "nine")
    (else (error "number must be in range 0..9"))))

(provide number->digits digit->word)
