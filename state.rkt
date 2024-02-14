
#lang racket

(require "command.rkt")
(require "structures.rkt")
(require "ring.rkt")

;; Program state class

(define/contract state%
  (class/c (init-field (var/-1 var?))
           (field (active-ring ring-name/c)
                  (operations-ring ring/c)
                  (math-ring ring/c)
                  (memory-position integer?))
           [get-active-ring (->m ring/c)]
           [get-inactive-ring (->m ring/c)]
           [swap-ring (->m void?)]
           [swap-ring-with-command (->m string?)]
           [spin-to (->m command? string?)]
           [execute (->m command? string?)]
           [move-memory (->m integer? string?)]
           [normalize-rings (->m string?)])
  (class object%
    (super-new)
    (init-field var/-1)
    (field [active-ring 'operations]
           [operations-ring (new ring% [name 'operations] [size ring-size])]
           [math-ring (new ring% [name 'math] [size ring-size])]
           [memory-position 0]) ;; Should generally be even. Movements to odd positions are implementation details.
    (define/public (get-active-ring)
      (case active-ring
        [(operations) operations-ring]
        [(math) math-ring]
        [else (error "Bad ring type: ~a" active-ring)]))
    (define/public (get-inactive-ring)
      (case active-ring
        [(operations) math-ring]
        [(math) operations-ring]
        [else (error "Bad ring type: ~a" active-ring)]))
    (define/public (swap-ring)
      (set! active-ring (if (eq? active-ring 'operations) 'math 'operations)))
    (define/public (swap-ring-with-command)
      (let ([old-active-ring (get-active-ring)])
        (swap-ring)
        (string-append
         (send old-active-ring rotate-to noop)
         "00")))
    (define/public (spin-to command)
      (string-append
       (if (eq? active-ring (command-ring-name command)) "" (swap-ring-with-command))
       (send (get-active-ring) rotate-to (command-index command))))
    (define/public (execute command)
      (begin0
          (string-append (spin-to command) "00")
        (swap-ring)))
    (define/public (move-memory target-position)
      (begin0
          (cond
            [(= target-position memory-position)
             ""]
            [(> target-position memory-position)
             (apply string-append
                    (execute op/set-to-one)
                    (for/list ([i (range memory-position target-position)])
                      (execute op/add-memory)))]
            [(< target-position memory-position)
             (let* ([<-1>-var-index (var-index var/-1)]
                    [distance-to-destination (- <-1>-var-index target-position)])
               (apply string-append
                      (move-memory <-1>-var-index)
                      (execute op/load-memory)
                      (for/list ([i (range distance-to-destination)])
                        (execute op/add-memory))))])
        (set! memory-position target-position)))
    (define/public (normalize-rings)
      ;; Resets both rings to the default state (both at noop facing
      ;; clockwise and the pointer on 'operations).
      (begin0
          (string-append
           (send (get-active-ring) rotate-to noop)
           "00"
           (send (get-inactive-ring) rotate-to noop)
           (if (eq? active-ring 'operations) "00" ""))
        (set! active-ring 'operations)))))

(provide state%)
