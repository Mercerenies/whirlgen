
#lang racket

(define/contract (number->digits n)
  (-> natural-number/c list?)
  (number->digits-rec n '()))

(define (number->digits-rec n acc)
  (if (zero? n)
      acc
      (number->digits-rec (quotient n 10) (cons (modulo n 10) acc))))

(provide number->digits)
