
#lang racket

;; Miscellaneous small structures that don't need their own module.

(struct var (name index))

(provide
 (contract-out
  [struct var
    ((name symbol?)
     (index integer?))]))
