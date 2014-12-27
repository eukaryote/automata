#lang racket

(require rackunit)
(require "automata.rkt")
(require "machines.rkt")

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
  "run-fsm deterministic tests"
  (check-eq? (run-fsm door-dfsm '(neither rear)) 'closed)
  (check-eq? (run-fsm door-dfsm '()) 'closed)
  (check-eq? (run-fsm door-dfsm '(front)) #f)

  (check-eq?
    (run-fsm ends-with-even-number-of-zeros-dfsm '())
    #f)
  (check-eq?
    (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "01"))
    #f)
  (check-eq?
    (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "100"))
    'yes)
  (check-eq?
    (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "10010"))
    #f)
  (check-eq?
    (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "101100"))
    'yes)

  (check-eq?
    (run-fsm modulo-three-counter-dfsm (list 0 1 0 1 0 1 0 0))
    'q0)
  (check-eq?
    (run-fsm modulo-three-counter-dfsm (list 2 2))
    #f)
  (check-eq?
    (run-fsm modulo-three-counter-dfsm (list 2 2 2))
    'q0)
  (check-eq?
    (run-fsm modulo-three-counter-dfsm (list 1 'reset))
    'q0)
)

(test-case
  "run-fsm nondeterministic tests"
  (check-equal?
    (run-fsm contains-101-or-11-ndfsm (list 0 0 1))
    #f)
  (check-equal?
    (run-fsm contains-101-or-11-ndfsm (list 1 1))
    (set 'q4))
  (check-equal?
    (run-fsm missing-letter-ndfsm '())
    (set 'q1 'q2 'q3 'q4))
  (check-equal?
    (run-fsm missing-letter-ndfsm (string->list "abaccbacc"))
    (set 'q4))
)

(test-case
  "run-transducer tests"

  (check-equal?
    (run-transducer flip-bits-transducer (list 1 0 1 0 0) #t)
    (cons #f (list 0 1 0 1 1)))
  (check-equal?
    (run-transducer duplicate-transducer (list 0 1 1) #t)
    (cons 'q0 (list 0 0 1 1 1 1)))
  (check-equal?
    (run-transducer zero-prefixing-transducer (list 0) #t)
    (cons 'q0 (list 0 0)))
  (check-equal?
    (run-transducer zero-prefixing-transducer (list 1) #t)
    (cons 'q0 (list 0 1)))
  (for-each
    (lambda (pr)
      (check-equal?
        (run-transducer odd-parity-bit-transducer (car pr) #t)
        (cons 'q0 (cdr pr))))
    `(((0 0 0 0) . (0 0 0 0 1))
      ((0 0 0 1) . (0 0 0 1 0))
      ((0 0 1 0) . (0 0 1 0 0))
      ((0 0 1 1) . (0 0 1 1 1))
      ((0 1 0 0) . (0 1 0 0 0))
      ((0 1 0 1) . (0 1 0 1 1))
      ((0 1 1 0) . (0 1 1 0 1))
      ((0 1 1 1) . (0 1 1 1 0))
      ((1 0 0 0) . (1 0 0 0 0))
      ((1 0 0 1) . (1 0 0 1 1))
      ((1 0 1 0) . (1 0 1 0 1))
      ((1 0 1 1) . (1 0 1 1 0))
      ((1 1 0 0) . (1 1 0 0 1))
      ((1 1 0 1) . (1 1 0 1 0))
      ((1 1 1 0) . (1 1 1 0 0))
      ((1 1 1 1) . (1 1 1 1 1)))))


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
