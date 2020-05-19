#lang racket
(require rackunit rackunit/text-ui "lunar-defns.rkt" "lunar.rkt")

(define (stationary-ship-at-height h)
  (ship h 0.0 20.0))

(define full-burn (lambda (state) 1))
(define no-burn (lambda (state) 0))

(define epsilon 0.01)
(define (close-enough? desired actual)
  (< (abs (- desired actual)) epsilon))

(define (constant-burn c)
  (lambda (state)
    c))

(define-check (check-equal-with-exn? val thunk)
  (with-handlers
      ([exn:fail? (lambda (exn)
                    (fail-check (string-append "an error was raised: " (exn-message exn))))])
    (let ([ret (thunk)])
      (unless (equal? ret val)
        (fail-check (string-append "expected " (format "~v" val) " got " (format "~v" ret)))))))

(define (burns-within-bounds? strat)
  (lambda (state)
    (and
     (>= (strat state) 0.0)
     (<= (strat state) 1.0)
     (>= (ship-fuel
          (update state (strat state))) ;ship-fuel of the new ship is now 0
         0.0))))

;; Go figure this doesn't remove the actual boilerplate
(define (successfully-true? my-thunk)
  (check-equal-with-exn? #t my-thunk))
(define clamp-tests
  (test-suite
   "Clamp works properly"
   (successfully-true? (thunk ((burns-within-bounds? (clamp full-burn)) initial-ship)))
   (successfully-true? (thunk ((burns-within-bounds? (clamp full-burn)) (ship 50 0 0.24))))
   (successfully-true? (thunk ((burns-within-bounds? (clamp (constant-burn 5))) initial-ship)))
   (successfully-true? (thunk ((burns-within-bounds? (clamp (constant-burn -1))) initial-ship)))))
(define (average-checker a b)
  (let ([true-average (/ (+ a b) 2)])
    (thunk
     (close-enough? true-average
                    ((average-strat (constant-burn a) (constant-burn b)) initial-ship)))))

(define average-tests
  (test-suite
   "Average strat works properly"
   (successfully-true? (average-checker 0 1))
   (successfully-true? (average-checker 0 0))
   (successfully-true? (average-checker 1 1))))

(define simple-height
  ;; We eta-expand this to avoid an "Implement me!" error at definition time.
  ;; Eta expansion means turning f into (lambda (x) (f x)). This is useful if "f" is really an expression that might
  ;; have side effects, like throwing an exception, as it can in our case.
  (lambda (s)
    ((height-choice no-burn full-burn 30) s)))

(define height-choice-tests
  (test-suite
   "Height choice works"
   (check-equal-with-exn? 0 (thunk (simple-height (stationary-ship-at-height (+ 30 epsilon)))))
   (check-equal-with-exn? 1 (thunk (simple-height (stationary-ship-at-height (- 30 epsilon)))))))



(define choice-tests
  (let ([always-true (lambda (s) #t)]
        [always-false (lambda (s) #f)])
    
    (test-suite
     "choice works"
     (check-equal-with-exn? 1 (thunk ((choice full-burn no-burn always-true) initial-ship)))
     (check-equal-with-exn? 0 (thunk ((choice full-burn no-burn always-false) initial-ship))))))

(define (run-to-conclusion state strat)
  (let loop ([state state])
    (if (landed? state)
        state
        (loop (update state (strat state))))))

(define (final-velocity initial strat)
  (let ([final-state (run-to-conclusion initial strat)])
    (ship-velocity final-state)))

(define (constant-below h)
  (clamp (height-choice no-burn constant-acc h)))

(define constant-acc-tests
  (test-suite
   "Delayed constant accelaration lands safely."
   (successfully-true? (thunk (landed-safely? (run-to-conclusion initial-ship (constant-below 40)))))
   (successfully-true? (thunk (landed-safely? (run-to-conclusion initial-ship (constant-below 30)))))
   (check-equal-with-exn? #f (thunk (landed-safely? (run-to-conclusion initial-ship (constant-below 20)))))
   (successfully-true? (thunk (close-enough? 0.5 (constant-acc initial-ship))))
   (successfully-true? (thunk (close-enough? 0.9 (constant-acc (ship 5 -2 20)))))
   (successfully-true? (thunk (close-enough? 0.54 (constant-acc (ship 50 -2 20)))))))

(define optimal-constant-acc-tests
  (test-suite
   "Optimal constant acceleration works."
   (check-equal-with-exn? 0 (thunk (optimal-constant-acc initial-ship)))
   (check-equal-with-exn? 0 (thunk (optimal-constant-acc (ship 30 -4.5 20))))
   (successfully-true? (thunk (landed-safely? (run-to-conclusion initial-ship (clamp optimal-constant-acc)))))
   ;; The test for optimality has to be very generous, since the simulation is so inaccurate.
   (successfully-true? (thunk (let ([comparison-fuel
                                           (ship-fuel (run-to-conclusion initial-ship (constant-below 40)))]
                                          [supposedly-optimal-fuel
                                           (ship-fuel (run-to-conclusion initial-ship (clamp optimal-constant-acc)))])
                                      (> supposedly-optimal-fuel comparison-fuel))))))

(define (first-strategy-chosen? s1 s2)
  (equal? s1 (best-strategy s1 s2)))

(define best-strategy-tests
  (test-suite
   "Does best-strategy choose the best strategy?"
   (successfully-true? (thunk (first-strategy-chosen? (constant-below 40) no-burn)))
   (successfully-true? (thunk (first-strategy-chosen? (constant-below 30) (constant-below 40))))
   (successfully-true? (thunk (first-strategy-chosen? optimal-constant-acc (constant-below 40))))))

(run-tests
 (test-suite "all tests"
             clamp-tests average-tests height-choice-tests choice-tests optimal-constant-acc-tests best-strategy-tests))

