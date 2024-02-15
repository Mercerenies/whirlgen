
#lang racket

(require threading)

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

(define/contract nl
  code/c
  (new newline-code%))

(define (code #:comment [comment #f] . objects)
  (->* () (#:comment string?) #:rest (listof (or/c string? code/c)) code/c)
  (let* ([code-blocks (map (λ (obj) (if (string? obj) (new string-code% [text obj]) obj)) objects)]
         [code-object (if (= (length code-blocks) 1)
                          (first code-blocks)
                          (new list-code% [body code-blocks]))])
    (if comment
        (new commented-code% [code code-object] [comment comment])
        code-object)))

(provide
 code<%> code/c
 nl code)
