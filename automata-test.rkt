#lang racket

(require rackunit)
(require "automata.rkt")

;; A transition function (from Sipser) for a supermarket-style automatic door
;; that can be open closed and receives inputs for whether there are
;; people in front or behind the doors or both or neither.
(define door-func
  (table->lookup
    '((open (neither . closed) (front . open) (rear . open) (both . open))
      (closed (front . open) (neither . closed) (rear . closed) (both . closed)))))

;; Door FSM based using above transition function.
(define door-dfsm
  (make-fsm
    '(open closed)
    '(front rear both neither)
    'closed
    '(closed)
    door-func))

;; An FSM over #\0 and #\1 that accepts if the input
;; ends with an even number of zeros (at least 2).
(define ends-with-even-number-of-zeros-dfsm
  (make-fsm
    '(no maybe yes)
    `(#\0 #\1)
    'no
    '(yes)
    (table->lookup
      `((no (,#\0 . maybe) (,#\1 . no))
        (maybe (,#\0 . yes) (,#\1 . no))
        (yes (,#\0 . maybe) (,#\1 . no))))))

;; A nondeterministic FSM (from Rich) over "abcd" that accepts
;; if at least one of the letters is not in the input.
;; It works by epsilon-transitioning to each of the four
;; non-start states (which all accept), and each of those defines just a
;; self-transition for a different subset of 3 of the 4 characters,
;; so the machine effectively runs 4 copies of itself, each of which dies
;; when one of the four letters are seen.
(define missing-letter-ndfsm
  (make-fsm
    '(q0 q1 q2 q3 q4)
    (string->list "abcd")
    'q0
    '(q1 q2 q3 q4)
    (lambda (state input)
      (cond [(and (eq? 'q1 state) (not (eq? input #\a))) (set 'q1)]
            [(and (eq? 'q2 state) (not (eq? input #\b))) (set 'q2)]
            [(and (eq? 'q3 state) (not (eq? input #\c))) (set 'q3)]
            [(and (eq? 'q4 state) (not (eq? input #\d))) (set 'q4)]
            [#t (set)]))
    (lambda (state) (if (eq? 'q0 state) '(q1 q2 q3 q4) '()))))

;; A nondeterministic FSM (adapted from Sipser) over #\0 and #\1 that accepts
;; iff the input string contains 101 or 11.
(define contains-101-or-11-ndfsm
  (make-fsm
    '(q1 q2 q3 q4)
    (list 0 1)
    'q1
    '(q4)
    (lambda (state symbol)
      (cond [(eq? 'q1 state) (if (eq? 0 symbol)
                                 (list 'q1)
                                 (list 'q1 'q2))]
            [(eq? 'q2 state) (if (eq? 0 symbol)
                                 (list 'q3 'q2)
                                 '())]
            [(eq? 'q3 state) (if (eq? 1 symbol)
                                 (list 'q4)
                                 '())]
            [(eq? 'q4 state) 'q4]))
    (lambda (state) (if (eq? state 'q2) (list 'q3) '()))))

;; An FSM over #\0 and #\1 that accepts if it gets a non-empty input
;; with the same symbol used for first and last.
(define first-matches-last-dfsm
  (make-fsm
    '(s q1 q2 r1 r2)
    '(a b)
    's
    '(q1 r1)
    (table->lookup
      '((s (a . q1) (b . r1))
        (q1 (a . q1) (b . q2))
        (q2 (a . q1) (b . q2))
        (r1 (a . r2) (b . r1))
        (r2 (a . r2) (b . r1))))))

;; An FSM that accepts if the sum of the digits since the last reset (or since
;; the beginning if there is no reset) is congruent with 0 modulo 3.
(define modulo-three-counter-dfsm
  (make-fsm
    '(q0 q1 q2)
    (list 0 1 2 'reset)
    'q0
    '(q0)
    (table->lookup
      `((q0 (0 . q0) (1 . q1) (2 . q2) (reset . q0))
        (q1 (0 . q1) (1 . q2) (2 . q0) (reset . q0))
        (q2 (0 . q2) (1 . q0) (2 . q1) (reset . q0))))))


  ;; A Moore machine (outputs solely based on state) transducer that just
  ;; echoes back the inverse of each bit read (0 goes to 1, and vice versa).
(define flip-bits-transducer
  (make-transducer
    '(q0 q1)
    (list 0 1)
    (list 0 1)
    'q0
    '()
    (table->lookup
      `((q0 (,0 . q0) (,1 . q1))
        (q1 (,0 . q0) (,1 . q1))))
    (lambda (state)
      (if (eq? 'q0 state) (list 1) (list 0)))))


; A Mealy machine transducer (outputs based on transition) that adds an odd
; parity bit after each fourth binary digits (i.e., each group of 4 bits in
;; the input corresponds to 5 bits of output that include an odd parity bit).
;; This is an example in _Automata, Computability, and Complexity_, by Rich.
(define odd-parity-bit-transducer
  (make-transducer
    '(q0 q10 q11 q20 q21 q30 q31)
    (list 0 1)
    (list 0 1)
    'q0
    '(q0)
    (table->lookup
      `((q0  (,0 . q10) (,1 . q11))
        (q10 (,0 . q20) (,1 . q21))
        (q11 (,0 . q21) (,1 . q20))
        (q20 (,0 . q30) (,1 . q31))
        (q21 (,0 . q31) (,1 . q30))
        (q30 (,0 . q0) (,1 . q0))
        (q31 (,0 . q0) (,1 . q0))))
    (table->lookup
      `((q0  (,0 . (,0))    (,1 . (,1)))
        (q10 (,0 . (,0))    (,1 . (,1)))
        (q11 (,0 . (,0))    (,1 . (,1)))
        (q20 (,0 . (,0))    (,1 . (,1)))
        (q21 (,0 . (,0))    (,1 . (,1)))
        (q30 (,0 . (,0 ,1)) (,1 . (,1 ,0)))
        (q31 (,0 . (,0 ,0)) (,1 . (,1 ,1)))))))

;; transducer that echoes its binary input
(define identity-transducer
  (make-transducer
    '(q0)
    (list 0 1)
    (list 0 1)
    'q0
    '(q0)
    (table->lookup
      `((q0 (,0 . q0) (,1 . q0))))
    (table->lookup
      `((q0 (,0 . (,0)) (,1 . (,1)))))))

;; transducer that echoes binary input in duplicate
(define duplicate-transducer
  (make-transducer
    '(q0)
    (list 0 1)
    (list 0 1)
    'q0
    '(q0)
    (table->lookup
      `((q0 (,0 . q0) (,1 . q0))))
    (table->lookup
      `((q0 (,0 . (,0 ,0)) (,1 . (,1 ,1)))))))

;; transducer that outputs each binary input prefixed by a 0
(define zero-prefixing-transducer
  (make-transducer
    '(q0)
    (list 0 1)
    (list 0 1)
    'q0
    '(q0)
    (table->lookup
      `((q0 (,0 . q0) (,1 . q0))))
    (table->lookup
      `((q0 (,0 . (,0 ,0)) (,1 . (,0 ,1)))))))

;; testing helper that returns two closures, the first a function that can be
;; used as an echoing display function for a mealy machine, and the second which
;; returns a list of all calls made to the first function.
(define (make-mealy-display-recorder)
  (define calls '())
  (define (display state symbol)
    (set! calls (cons (cons state symbol) calls))
    (list symbol))
  (define (get-calls)
    (reverse calls))
  (values display get-calls))

;; testing that generated transition functions behave as expected
(check-equal? 'closed (door-func 'open 'neither))
(check-equal? 'open (door-func 'open 'front))
(check-equal? 'open (door-func 'open 'rear))
(check-equal? 'open (door-func 'open 'both))
(check-equal? 'open (door-func 'closed 'front))
(check-equal? 'closed (door-func 'closed 'rear))
(check-equal? 'closed (door-func 'closed 'both))
(check-equal? 'closed (door-func 'closed 'neither))

;; some run-fsm tests
(check-eq? (run-fsm door-dfsm '(neither rear)) 'closed)
(check-eq? (run-fsm door-dfsm '()) 'closed)
(check-eq? (run-fsm door-dfsm '(front)) #f)

(check-eq? (run-fsm ends-with-even-number-of-zeros-dfsm '()) #f)
(check-eq? (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "01")) #f)
(check-eq? (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "100")) 'yes)
(check-eq? (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "10010")) #f)
(check-eq? (run-fsm ends-with-even-number-of-zeros-dfsm (string->list "101100")) 'yes)

(check-eq? (run-fsm modulo-three-counter-dfsm (list 0 1 0 1 0 1 0 0)) 'q0)
(check-eq? (run-fsm modulo-three-counter-dfsm (list 2 2)) #f)
(check-eq? (run-fsm modulo-three-counter-dfsm (list 2 2 2)) 'q0)
(check-eq? (run-fsm modulo-three-counter-dfsm (list 1 'reset)) 'q0)

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
    ((1 1 1 1) . (1 1 1 1 1))))

(require/expose "automata.rkt" (make-output-helpers))


;; testing in streaming mode
(let-values ([(transition-output make-result)
              (make-output-helpers identity-transducer)])
  (check-true (procedure? transition-output))
  (check-true (procedure? make-result))
  (check-equal? (make-result 'q0) 'q0)
  (transition-output 'q0 1 'q0)
  (check-equal? (make-result 'q0) 'q0))

(let-values ([(display get-display-calls)
              (make-mealy-display-recorder)])
  (let ([t (make-transducer '(q0 q1)
                            (list 0 1)
                            (list 0 1)
                            'q0
                            '(q0)
                            (table->lookup
                              `((q0 (,0 . q0) (,1 . q0))))
                            display)])
    (let-values ([(transition-output make-result)
                  (make-output-helpers t)])
      ;; verify output  is just the accepting state
      (check-equal? (run-transducer t (list 0 1 1 0 1)) 'q0)
      ;; verify that the display function was called with the expected args
      (check-equal? (get-display-calls) `((q0 . ,0) (q0 . ,1) (q0 . ,1) (q0 . ,0) (q0 . ,1))))))

;; testing in non-streaming mode
(let-values ([(transition-output make-result)
              (make-output-helpers identity-transducer #t)])
  (check-true (procedure? transition-output))
  (check-true (procedure? make-result))
  (check-equal? (make-result 'q0) '(q0))
  (transition-output 'q0 1 'q0)
  (check-equal? (make-result 'q0) (list 'q0 1))
  (transition-output 'q0 0 'q0)
  (check-equal? (make-result 'q0) (list 'q0 1 0)))

(let-values ([(transition-output make-result)
              (make-output-helpers duplicate-transducer #t)])
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

(let ([states '(q0 q1 q2 q3)])
  (let-values
    ([(state->index index->state fold-active-states map-active-states reset-active-states! update-active-states!)
     (make-state-helpers states '(q1))])
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
    (check-equal? (fold-active-states
                    (lambda (state acc) (set-add acc state))
                    (set))
                  (list->set '(q0 q3)))))

(check-equal? (run-fsm contains-101-or-11-ndfsm (list 0 0 1)) #f)
(check-equal? (run-fsm contains-101-or-11-ndfsm (list 1 1)) (set 'q4))
(check-equal? (run-fsm missing-letter-ndfsm '()) (set 'q1 'q2 'q3 'q4))
(check-equal? (run-fsm missing-letter-ndfsm (string->list "abaccbacc")) (set 'q4))
