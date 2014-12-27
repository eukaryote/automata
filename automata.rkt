#lang racket

(require racket/stream)
(require data/bit-vector)
(require data/gvector)

(provide make-fsm make-transducer run-fsm run-transducer table->lookup)

;;;; Public constructors

(define (make-fsm states input-alphabet start-state accepting-states transition [nondeterministic #f])
  ;; verify inputs and convert to sets if needed before creating fsm
  (let ([s (as-set states)]
        [ia (as-set input-alphabet)]
        [ss start-state]
        [as (as-set accepting-states)]
        [t transition]
        [nd nondeterministic])
    (verify-start-state ss s)
    (verify-accepting-states as s)
    (fsm s ia ss as t nd)))

(define (make-transducer states input-alphabet output-alphabet start-state accepting-states transition display)
  ;; verify inputs and convert to sets if needed before creating transducer
  (let ([s (as-set states)]
        [ia (as-set input-alphabet)]
        [oa (as-set output-alphabet)]
        [ss start-state]
        [as (as-set accepting-states)]
        [t transition]
        [d display])
    (verify-start-state ss s)
    (verify-accepting-states as s)
    (transducer s ia oa ss as t d)))

;;;; Public functions for running an automaton

;; Run the fsm with the given list (or any stream) of inputs, returning the
;; accepting state if it finishes in an accepting state, or #f if it finishes
;; in a non-accepting state.
(define (run-fsm fsm inputs)
  (let ([run (if (fsm-nondeterministic fsm)
                 run-nondeterministic-fsm
                 run-deterministic-fsm)])
    (run fsm inputs)))

;; Run the transducer with the given list (or any stream) of inputs, returning
;; the accepting state (or #f if it doesn't finish in an accepting state) if
;; collect-output is #f, or a pair consisting of the accepting state (or #f)
;; and a list of the concatenated results of the output(s) for each input
;; if collect-output is not #f.
(define (run-transducer t inputs [collect-output #f])
  ;; aliases
  (define input-alphabet (transducer-input-alphabet t))
  (define all-states (transducer-states t))
  (define transition (transducer-transition t))
  ;; output helpers for recording each output and making the final result
  (define-values (transition-output make-result)
                 (make-output-helpers t collect-output))
  ;; verify and get the next input from the non-empty list of inputs
  (define (next-input inputs)
    (let ([input (stream-first inputs)])
      (verify-input input input-alphabet)
      input))
  ;; run over inputs, returning result after all inputs consumed
  (define (run-iter current-state inputs)
    (if (stream-empty? inputs)
        (make-result current-state)
        (let* ([input (next-input inputs)]
               [next-state (transition current-state input)])
          (verify-transition-state next-state all-states)
          (transition-output current-state input next-state)
          (run-iter next-state (stream-rest inputs)))))
  (run-iter (transducer-start-state t) (as-input-stream inputs)))

;; Public constants and utility functions

;; Convert a table representation of a lookup function (e.g., for a transition
;; function or a display/output function) into an actual lookup function
;; for use with an fsm or transducer.
;;
;; Given a list of lists, each inner list having the following form:
;;    (key1 (key1a . x) (key1b . y) ...)
;; The generated lookup function would answer x when given key1 and key1a as
;; args, would answer y when given key1 and key1b as args, etc.
;;
;; For a transition function, key1 would be a current state, key1a would be an
;; input, and x would be a next state or a set of next states. For an
;; output/display function of a Mealy machine, for example, key1 and key1a
;; would have the same semantics, except for x and y being a set of outputs
;; for a given transition.
(define (table->lookup table)
  (let ([ht (make-hash)])
    ; build hash table that maps a pair key to a value; each row in the table
    ; yields as many entries in the table as there are pairs in the row.
    (for-each
      (lambda (row)
        (let ([key (car row)])  ;; first elem is the first part of the key
          (for-each
            (lambda (pr)
              ;; the first part of each pair if the second part of the key
              ;; for the lookup function, and the second part of each pair
              ;; is the result for the combined key.
              (hash-set! ht (cons key (car pr)) (cdr pr)))
            (cdr row))))
      table)
    (lambda (key1 key2)
      (hash-ref ht (cons key1 key2)))))


;; Structs for each kind of supported automaton (constructors are private)

(struct fsm
  (;; a finite set of states (Q):
   states
   ;; a finite set of symbols (\Sigma) that can be inputs:
   input-alphabet
   ;; an initial state q_0, which is an element of Q
   start-state
   ;; a finite set of states (A), which is a subset of Q
   accepting-states
   ;; a transition function (\delta), Q x \Sigma -> Q if deterministic
   ;; or Q x \Sigma -> Q* if non-deterministic
   transition
   ;; whether fsm is nondeterministic; may be #t for a non-deterministic
   ;; fsm that doesn't use epsilon transitions, or a function of one argument
   ;; that should return a (possibly empty) list or set of next states to be
   ;; transitioned to by an epsilon transition for a given state that will be
   ;; passed as the argument.
   nondeterministic))

(struct transducer
  (;; a finite set of states (Q):
   states
   ;; a finite set of symbols (\Sigma) that can be inputs:
   input-alphabet
   ;; a finite set of symbols (\Lambda) that can be outputs:
   output-alphabet
   ;; an initial state q_0, which is an element of Q
   start-state
   ;; a finite set of states (A), which is a subset of Q
   accepting-states
   ;; a transition function (\delta), of type Q x \Sigma -> Q
   transition
   ;; a display function (D), which is called with an element from either
   ;; Q or Q x \Sigma, depending on the type of transducer, and should
   ;; return a set of output symbols
   display))


;;;; Private implementations

(define (run-deterministic-fsm fsm inputs)
  (define (run current-state symbols)
    (if (stream-empty? symbols)
        (if (set-member? (fsm-accepting-states fsm) current-state) current-state #f)
        (let ([symbol (stream-first symbols)])
          (when (not (set-member? (fsm-input-alphabet fsm) symbol))
            (error (format "invalid input: ~v" symbol)))
          (let ([next-state ((fsm-transition fsm) current-state symbol)])
            (when (not (set-member? (fsm-states fsm) next-state))
              (error (format "invalid transition result: ~v" next-state)))
            (run next-state (stream-rest symbols))))))
  (run (fsm-start-state fsm) (as-input-stream inputs)))


(define (expand-recursively base-elems expander)
  (let ([expanded-elems base-elems])
    (for ([elem base-elems])
      (set! expanded-elems (set-union expanded-elems (as-set (expander elem)))))
    (if (not (eq? (set-count expanded-elems) (set-count base-elems)))
        (expand-recursively expanded-elems expander)
        expanded-elems)))

;; Get the transitive closure of all states that are reachable from
;; current-states via epsilon transitions.
(define (epsilon-closure fsm current-states)
  (let ([states (as-set current-states)]
        [expander (fsm-nondeterministic fsm)])
    (if (procedure? expander)
        (expand-recursively states expander)
        states)))

(define (run-nondeterministic-fsm fsm inputs)
  (define-values (state->index
                  index->state
                  fold-active-states
                  map-active-states
                  reset-active-states!
                  update-active-states!)
     (make-state-helpers (fsm-states fsm) (list (fsm-start-state fsm))))
  (define (expand-epsilon-states)
    (update-active-states! (epsilon-closure fsm (map-active-states identity))))
  (define (make-result)
    (let ([final-states
      (set-intersect (list->set (map-active-states identity))
                      (fsm-accepting-states fsm))])
    (if (set-empty? final-states) #f final-states)))
  (define (run inputs)
    (if (stream-empty? inputs)
        (expand-epsilon-states)  ; nothing but epsilon-expansion if no input
        (for ([input inputs])    ; otherwise, for each input, expand, transition, and update
          (expand-epsilon-states)
          (reset-active-states!
            (fold-active-states
              (lambda (state acc) (set-union acc (as-set ((fsm-transition fsm) state input))))
              (set))))))
  (run (as-input-stream inputs))
  (make-result))

;; Convert inputs to a stream if needed, or raise error if not a supported
;; input format (string is currently the only non-stream format supported).
(define (as-input-stream inputs)
  (cond [(stream? inputs) inputs]
        [(string? inputs) (string->list inputs)]
        [#t (error "input format must be a stream or a string")]))

; For a given transducer t, generate two helper functions (referred to as
; 'transition-output' and 'make-result' below) that handle processing the
; incremental outputs of a transducer and generating the final result of the
; transducer given the last state.
;
; Depending on whether collect-output is #f or not, the two helpers operate in
; either a streaming mode, where each 'transition-output' call merely invokes
; the user-specified 'display' function (outputs aren't collected) and the
; 'make-result' function just returns the accepting state (if any), or if
; collect-output is a true value, then the 'display' function accumulates the
; outputs in addition to calling the user-specified 'display' function, and
; 'make-result' returns a pair consisting of the accepting state (if any) and a
; list of the concatenated results of the output for each symbol. The streaming
; mode uses a constant amount of memory, and so is preferable for handling
; large inputs.

; The two functions returned as multiple values are:
; - transition-output: (current-state x symbol x next-state) -> outputs
; - make-result: () -> accepting-states-or-false union
;                      (accepting-states-or-false . concatenated-outputs)
(define (make-output-helpers t [collect-output #f])
  ; output list of symbols that will be consed on in reversed order (e.g.,
  ; for out of 0 1 on first transition and 1 2 on second transition, they
  ; would be consed on as 1 0, and then 2 1), and will be reversed upon get.
  (define OUT '())
  ;; some aliases
  (define alphabet (transducer-output-alphabet t))
  (define user-display (transducer-display t))
  (define arity (procedure-arity user-display))
  ;; verify arity just once
  (unless (or (eq? arity 1) (eq? arity 2))
    (error (format "transducer-display arity should be 1 or 2, not ~v" arity)))
  (define user-display-args
    (if (eq? arity 1)
        (lambda (current-state symbol next-state)
          (list next-state))
        (lambda (current-state symbol next-state)
          (list current-state symbol))))
  (define (transition-output current-state symbol next-state)
    (let ([outputs (apply user-display (user-display-args current-state symbol next-state))])
      (verify-outputs outputs alphabet)
      (when collect-output
        (for-each (lambda (output) (set! OUT (cons output OUT))) outputs))
      outputs))
  (define (make-result last-state)
    (let ([state-result (if (set-member? (transducer-accepting-states t) last-state) last-state #f)]
          [output (reverse OUT)])
      (if collect-output
          (cons state-result output)
          state-result)))
  (values transition-output make-result))


(define (make-state-helpers states [active-states '()])
  ; shared state and convenience functions that are used by the closures
  ; that are returned by this function
  (define state-vector
    (cond [(list? states) (list->vector states)]
          [(vector? states) states]
          [(set? states) (list->vector (set->list states))]))
  (define state-size (vector-length state-vector))
  (define state-bit-vector (make-bit-vector state-size))
  (define state-map (make-hasheq))
  (define index->state (lambda (index) (vector-ref state-vector index)))
  (define state->index (lambda (state) (hash-ref state-map state)))
  (define (fold-active-states proc init)
    (let ([result init])
      (for ([i (in-range state-size)]
            [active (in-bit-vector state-bit-vector)])
        (when active (set! result (proc (index->state i) result))))
      result))
  (define (map-active-states proc)
    (filter-map identity
      (for/list ([i (in-range state-size)]
                 [active (in-bit-vector state-bit-vector)])
        (if active (proc (index->state i)) #f))))
  (define (update-active-states! [states '()] [reset #f])
    (when reset (set! state-bit-vector (make-bit-vector state-size)))
    (for ([state states])
      (bit-vector-set! state-bit-vector (state->index state) #t)))
  (define (reset-active-states! [states '()])
    (update-active-states! states #t))
  ; initialize the state map
  (for ([index (in-range state-size)]
        [state (in-vector state-vector)])
    (hash-set! state-map state index))
  ; set the initially active states
  (for ([state active-states])
    (bit-vector-set! state-bit-vector (state->index state) #t))
  ; helper closures:
  (values
    state->index
    index->state
    fold-active-states
    map-active-states
    reset-active-states!
    update-active-states!))


(define (as-set list-or-set)
  (cond [(list? list-or-set) (list->set list-or-set)]
        [(set? list-or-set) list-or-set]
        [#t (error (format "not a list or set: ~v" list-or-set))]))

(define (verify-state state all-states error-message)
  (unless (set-member? all-states state)
    (error (format error-message state))))

(define (verify-start-state start-state all-states)
  (verify-state start-state all-states "invalid start-state: ~v"))

(define (verify-transition-state state all-states)
  (verify-state state all-states "invalid transition state: ~v"))

(define (verify-accepting-states accepting-states all-states)
  (let ([invalid-states (set-subtract accepting-states all-states)])
    (unless (set-empty? invalid-states)
      (error (format "invalid accepting states: ~v" invalid-states)))))

(define (verify-input input alphabet)
  (unless (set-member? alphabet input)
    (error (format "invalid input: ~v" input))))

(define (verify-outputs outputs alphabet)
  (let ([invalid (filter (lambda (output) (not (set-member? alphabet output))) outputs)])
    (unless (empty? invalid)
      (error (format "invalid outputs: ~v" invalid)))))
