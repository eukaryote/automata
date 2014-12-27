#lang racket

(require rackunit)
(require "automata.rkt")
(require "machines.rkt")

;; Check that fsm when run with given input has expected result.
(define-check (check-fsm? fsm input expected)
  (with-check-info (('name "check-fsm?")
                    ('params input)
                    ('expected expected)
                    ('message "Unexpected fsm result"))
    (let ([output (run-fsm fsm input)])
      (with-check-info (('actual output))
          (when (not (equal? output expected))
            (fail-check))))))

;; Check that transducer when run with given input has expected result.
(define-check (check-transducer? transducer input expected)
  (with-check-info (('name "check-transducer?")
                    ('params input)
                    ('expected expected)
                    ('message "Unexpected transducer result"))
    (let ([output (run-transducer transducer input #t)])
      (with-check-info (('actual output))
        (when (not (equal? output expected))
          (fail-check))))))

;; Testing helper that returns three functions, the first can be used as an
;; echoing display function for a mealy machine, the second returns a list
;; of all calls made to the first function, and the third will clear the
;; recorded calls.
(define (mealy-display-helpers)
  (define calls '())
  (define (display state symbol)
    (set! calls (cons (cons state symbol) calls))
    (list symbol))
  (define (get-calls) (reverse calls))
  (define (clear!) (set! calls '()))
  (values display get-calls clear!))


(test-case
  "table->lookup tests"
  (define delta (table->lookup door-fsm-table))
  (check-equal? (delta 'open 'neither) 'closed)
  (check-equal? (delta 'open 'neither) 'closed)
  (check-equal? (delta 'open 'front) 'open)
  (check-equal? (delta 'open 'rear) 'open)
  (check-equal? (delta 'open 'both) 'open)
  (check-equal? (delta 'closed 'front) 'open)
  (check-equal? (delta 'closed 'rear) 'closed)
  (check-equal? (delta 'closed 'both) 'closed)
  (check-equal? (delta 'closed 'neither) 'closed))

(test-case
  "run-fsm deterministic tests: door-fsm"
  (define fsm door-dfsm)
  (check-fsm? fsm '() 'closed)
  (check-fsm? fsm '(rear) 'closed)
  (check-fsm? fsm '(neither) 'closed)
  (check-fsm? fsm '(both) 'closed)
  (check-fsm? fsm '(front) #f)
  (check-fsm? fsm '(front neither) 'closed)
  (check-fsm? fsm '(front rear neither) 'closed)
  (check-fsm? fsm '(front neither both front) #f)
)

(test-case
  "run-fsm deterministic tests: ends-with-even-number-of-zeros-dfsm"
  (define fsm ends-with-even-number-of-zeros-dfsm)
  (check-fsm? fsm "" #f)
  (check-fsm? fsm "0" #f)
  (check-fsm? fsm "00" 'yes)
  (check-fsm? fsm "100" 'yes)
  (check-fsm? fsm "1001000" #f)
  (check-fsm? fsm "101100" 'yes)
  (check-fsm? fsm "10000" 'yes)
)

(test-case
  "run-fsm deterministic tests: modulo-three-counter-dfsm"
  (define fsm modulo-three-counter-dfsm)
  (check-fsm? fsm '() 'q0)
  (check-fsm? fsm '(0 1 0 1 0 1 0 0) 'q0)
  (check-fsm? fsm '(2 2) #f)
  (check-fsm? fsm '(2 2 2) 'q0)
  (check-fsm? fsm '(1 reset) 'q0)
  (check-fsm? fsm '(1 reset 2 2 2) 'q0)
)

(test-case
  "run-fsm nondeterministic tests: contains-101-or-11-ndfsm"
  (define fsm contains-101-or-11-ndfsm)
  (check-fsm? fsm '() #f)
  (check-fsm? fsm '(0) #f)
  (check-fsm? fsm '(1) #f)
  (check-fsm? fsm '(0 0 1 0 0) #f)
  (check-fsm? fsm '(1 0 0) #f)
  (check-fsm? fsm '(1 1) (set 'q4))
  (check-fsm? fsm '(1 0 1) (set 'q4))
  (check-fsm? fsm '(1 0 1 1) (set 'q4))
  (check-fsm? fsm '(1 1 0 1) (set 'q4))
  (check-fsm? fsm '(1 1 1 1 1) (set 'q4))
)

(test-case
  "run-fsm nondeterministic tests: missing-letter-ndfsm"
  (define fsm missing-letter-ndfsm)
  (check-fsm? fsm '() (set 'q1 'q2 'q3 'q4))
  (check-fsm? fsm "abcd" #f)
  (check-fsm? fsm "a" (set 'q2 'q3 'q4))
  (check-fsm? fsm  "b" (set 'q1 'q3 'q4))
  (check-fsm? fsm "c" (set 'q1 'q2 'q4))
  (check-fsm? fsm "d" (set 'q1 'q2 'q3))
  (check-fsm? fsm "abadaba" (set 'q3))
  (check-fsm? fsm "cada" (set 'q2))
)

(test-case
  "run-transducer tests: flip-bits-transducer"
  (define tx flip-bits-transducer)
  (check-transducer? tx '() (cons #f '()))
  (check-transducer? tx '(0) (cons #f '(1)))
  (check-transducer? tx '(1) (cons #f '(0)))
  (check-transducer? tx '(0 0) (cons #f '(1 1)))
  (check-transducer? tx '(0 1) (cons #f '(1 0)))
  (check-transducer? tx '(1 0) (cons #f '(0 1)))
  (check-transducer? tx '(1 1) (cons #f '(0 0)))
  (check-transducer? tx '(1 0 1 0 0 1 0 0 0) '(#f . (0 1 0 1 1 0 1 1 1)))
)

(test-case
  "run-transducer tests: duplicate-transducer"
  (define tx duplicate-transducer)
  (check-transducer? tx '() '(q0 . ()))
  (check-transducer? tx '(0) '(q0 . (0 0)))
  (check-transducer? tx '(1) '(q0 . (1 1)))
  (check-transducer? tx '(1 1) '(q0 . (1 1 1 1)))
  (check-transducer? tx '(0 1 0 1) '(q0 . (0 0 1 1 0 0 1 1)))
)

(test-case
  "run-transducer tests: zero-prefixing-transducer"
  (define tx zero-prefixing-transducer)
  (check-transducer? tx '() '(q0 . ()))
  (check-transducer? tx '(0) '(q0 . (0 0)))
  (check-transducer? tx '(1) '(q0 . (0 1)))
  (check-transducer? tx '(0 1 1) '(q0 . (0 0 0 1 0 1)))
  (check-transducer? tx '(1 1 1 0) '(q0 . (0 1 0 1 0 1 0 0)))
)

(test-case
  "run-transducer tests: odd-parity-bit-transducer"
  (define tx odd-parity-bit-transducer)
  (check-transducer? tx '(0 0 0 0) '(q0 . (0 0 0 0 1)))
  (check-transducer? tx '(0 0 0 1) '(q0 . (0 0 0 1 0)))
  (check-transducer? tx '(0 0 1 0) '(q0 . (0 0 1 0 0)))
  (check-transducer? tx '(0 0 1 1) '(q0 . (0 0 1 1 1)))
  (check-transducer? tx '(0 1 0 0) '(q0 . (0 1 0 0 0)))
  (check-transducer? tx '(0 1 0 1) '(q0 . (0 1 0 1 1)))
  (check-transducer? tx '(0 1 1 0) '(q0 . (0 1 1 0 1)))
  (check-transducer? tx '(0 1 1 1) '(q0 . (0 1 1 1 0)))
  (check-transducer? tx '(1 0 0 0) '(q0 . (1 0 0 0 0)))
  (check-transducer? tx '(1 0 0 1) '(q0 . (1 0 0 1 1)))
  (check-transducer? tx '(1 0 1 0) '(q0 . (1 0 1 0 1)))
  (check-transducer? tx '(1 0 1 1) '(q0 . (1 0 1 1 0)))
  (check-transducer? tx '(1 1 0 0) '(q0 . (1 1 0 0 1)))
  (check-transducer? tx '(1 1 0 1) '(q0 . (1 1 0 1 0)))
  (check-transducer? tx '(1 1 1 0) '(q0 . (1 1 1 0 0)))
  (check-transducer? tx '(1 1 1 1) '(q0 . (1 1 1 1 1)))
)



(require/expose "automata.rkt" (make-output-helpers))

(test-case "streaming output helper tests"
  (define-values (display get-display-calls clear-calls!)
                 (mealy-display-helpers))
  ;; test transducer with an display function that echoes input but records
  ;; the calls, which can be retrieved for verification using get-display-calls
  (define transducer
    (make-transducer '(q0 q1) (list 0 1) (list 0 1) 'q0 '(q0)
                     (table->lookup `((q0 (,0 . q0) (,1 . q0))))
                     display))
  (define-values (transition-output make-result)
                 (make-output-helpers transducer))

  (check-true (procedure? transition-output))
  (check-true (procedure? make-result))
  (check-equal? (make-result 'q0) 'q0)
  (transition-output 'q0 1 'q0)
  (check-equal? (make-result 'q0) 'q0)

  (clear-calls!)

  ;; verify output  is just the accepting state
  (check-equal? (run-transducer transducer (list 0 1 1 0 1)) 'q0)

  ;; verify that the display function was called with the expected args
  (check-equal?
    (get-display-calls)
    `((q0 . ,0)
      (q0 . ,1)
      (q0 . ,1)
      (q0 . ,0)
      (q0 . ,1))))

(test-case
  "non-streaming output helper tests -- identity-transducer"
  (define-values (transition-output make-result)
                 (make-output-helpers identity-transducer #t))

  (check-true (procedure? transition-output))
  (check-true (procedure? make-result))
  (check-equal? (make-result 'q0) '(q0))
  (transition-output 'q0 1 'q0)
  (check-equal? (make-result 'q0) (list 'q0 1))
  (transition-output 'q0 0 'q0)
  (check-equal? (make-result 'q0) (list 'q0 1 0)))

(test-case
  "non-streaming output helper tests -- duplicate-transducer"
  (define-values (transition-output make-result)
                 (make-output-helpers duplicate-transducer #t))
  (check-equal? (make-result 'q0) '(q0))
  (transition-output 'q0 0 'q0)
  (check-equal? (make-result 'q0) '(q0 0 0))
  (transition-output 'q0 1 'q0)
  (transition-output 'q0 0 'q0)
  (transition-output 'q0 0 'q0)
  (transition-output 'q0 0 'q0)
  (transition-output 'q0 1 'q0)
  (check-equal? (make-result 'q0) '(q0 0 0 1 1 0 0 0 0 0 0 1 1)))

(require/expose "automata.rkt" (make-state-helpers))

(test-case
  "state helper tests"
  (define states '(q0 q1 q2 q3))
  (define-values
    (state->index index->state fold-active-states
      map-active-states reset-active-states! update-active-states!)
    (make-state-helpers states '(q1)))

  (check-equal? (state->index 'q0) 0)
  (check-equal? (state->index 'q2) 2)
  (check-equal? (index->state 3) 'q3)
  (check-equal? (index->state 1) 'q1)
  (check-equal? (map-active-states identity) '(q1))
  (reset-active-states! '())
  (check-equal? (map-active-states identity) '())
  (reset-active-states! '(q0 q3))
  (check-equal? (map-active-states identity) '(q0 q3))
  (check-equal? (map-active-states symbol->string) '("q0" "q3"))
  (check-equal?
    (fold-active-states (lambda (state acc) (set-add acc state))
                        (set))
    (list->set '(q0 q3)))
)
