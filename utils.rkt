
#lang racket

(define/contract (number->digits n #:radix [radix 10])
  (->* (natural-number/c) (#:radix natural-number/c) list?)
  (number->digits-rec n radix '()))

(define (number->digits-rec n radix acc)
  (if (zero? n)
      acc
      (number->digits-rec (quotient n radix) radix (cons (modulo n radix) acc))))

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

(define/contract (pad-right str n #:pad-char pad-char)
  (-> string? natural-number/c #:pad-char char? string?)
  (string-append str (make-string (max (- n (string-length str)) 0) pad-char)))

(provide number->digits digit->word pad-right)
