
#lang racket

(require threading)

(require "utils.rkt")

(define code<%>
  (interface ()
    [to-string (->m string?)]
    ;; Note: length only includes the characters actually considered
    ;; by the interpreter. The particular interpreter we're using
    ;; recognizes C-style "//" comments, so this count should exclude
    ;; C-style comments and trailing/leading whitespace.
    [length (->m integer?)]))

(define code/c (is-a?/c code<%>))

(define/contract string-code%
  (class/c (init-field (text string?)))
  (class* object% (code<%>)
    (super-new)
    (init-field text)
    (define/public (to-string)
      text)
    (define/public (length)
      (string-length text))))

(define/contract list-code%
  (class/c (init-field (body list?)))
  (class* object% (code<%>)
    (super-new)
    (init-field body)
    (define/public (to-string)
      (~>> body
           (map (λ (code) (send code to-string)))
           (apply string-append)))
    (define/public (length)
      (~>> body
           (map (λ (code) (send code length)))
           (apply +)))))

(define/contract newline-code%
  (class/c)
  (class* object% (code<%>)
    (super-new)
    (define/public (to-string)
      "\n")
    (define/public (length)
      0)))

(define/contract commented-code%
  (class/c (init-field (code code/c) (comment string?)))
  (class* object% (code<%>)
    (super-new)
    (init-field code comment)
    (define/public (to-string)
      (format "~a // ~a\n" (send code to-string) comment))
    (define/public (length)
      (send code length))))

(define/contract padded-code%
  (class/c (init-field (code code/c) (desired-length integer?)))
  (class* object% (code<%>)
    (super-new)
    (init-field code desired-length)
    (define/public (to-string)
      (let ([inner-code (send code to-string)])
        (if (> (string-length inner-code) desired-length)
            (error "Code is too long")
            (pad-right inner-code desired-length #:pad-char #\2))))
    (define/public (length)
      desired-length)))

(define/contract nl
  code/c
  (new newline-code%))

(define (code #:comment [comment #f] #:desired-length [desired-length #f] . objects)
  (->* () (#:comment (or/c string? #f) #:desired-length (or/c integer? #f))
       #:rest (listof (or/c string? code/c)) code/c)
  (let* ([code-blocks (map (λ (obj) (if (string? obj) (new string-code% [text obj]) obj)) objects)]
         [code-object (if (= (length code-blocks) 1)
                          (first code-blocks)
                          (new list-code% [body code-blocks]))]
         [padded-code (if desired-length
                          (new padded-code% [code code-object] [desired-length desired-length])
                          code-object)])
    (if comment
        (new commented-code% [code padded-code] [comment comment])
        padded-code)))

(provide
 code<%> code/c
 nl code)
