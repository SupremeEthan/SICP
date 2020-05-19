#lang racket

;; Imports definitions from another file. Here, we load the file named
;; `lunar-defns.rkt`. Go read it! The comments will help you understand the
;; program.
(require "lunar-defns.rkt")

;; The `provide` function makes these symbols public so other files can use
;; them.
;;
;; In particular, our tests are in a separate file and need to access the code
;; you wrote in this one.
;;
;; Here are the things you need to write:
(provide clamp average-strat height-choice choice ask-after-40
         constant-acc optimal-constant-acc best-strategy)

;; A strategy is a function which takes a ship state and returns a burn-rate.
;; These simple strategies either burn the thrusters at full strength, or not at
;; all.
;;
;; Arguments:
;;   state (ship?): The ship's current state.
;;
;; Returns (number?): The burn rate.
(define (full-burn state) 1)
(define (no-burn state) 0)

;; The ask-user strategy uses console input to determine if the thrusters should
;; be burned or not at each particular step.
(define (ask-user state)
  (if (ask-to-burn)
      (full-burn state)
      (no-burn state)))

;; Simulates a landing with the given strategy.
(define (play strat)
  (lander-loop initial-ship strat))

;; Simulates a landing with the `ask-user` strategy.
(define (play-interactive) (play ask-user))

;; A higher-order strategy which returns a "clamped" version of the given
;; strategy.
;;
;; This is a function which takes a strategy (a function) and returns another
;; strategy (a function). Functions that either take functions as arguments or
;; return them as results are called higher-order functions, and they help make
;; our programs composable and modular -- in other words, split into manageable
;; and maintainable chunks.
;;
;; Returns: a strategy which returns the physically plausible burn rate closest to strat's actual output.
(define (clamp strat)
  (lambda (state)
    (cond
      ;;burn rate must be plausible before comparing requested fuel with remaining fuel
      ((and (< (ship-fuel state) (strat state)) (<= (strat state) 1.0) (>= (strat state) 0.0)) (ship-fuel state))
      ((>= (strat state) 1.0) 1.0)
      ((<= (strat state) 0.0) 0.0)
      ; the burn-rate is physically plausible
      (else (strat state))))
  )

;; Returns: A strategy which returns the average of the two input strategies.
(define (average-strat strat1 strat2)
  (lambda (state)
    (/ (+ (strat1 state)
          (strat2 state)) 2)))

;; Returns: A strategy which uses `strat1` when the ship is above
;;   `cutoff-height` and `strat2` when the ship is below `cutoff-height`.
(define (height-choice strat1 strat2 cutoff-height)
  (lambda (state)
    (if (< (ship-height state) cutoff-height)
        (strat2 state)
        (strat1 state))))

;; Arguments:
;;  strat1: The first strategy.
;;  strat2: The second strategy.
;;  predicate (-> ship? bool?): A predicate on a ship state; if the predicate
;;    returns #t, use `strat1`.
;;
;; Returns: A strategy which uses `strat1` if `predicate` returns #t for a given
;;   ship-state, and `strat2` if `predicate` returns #f.
(define (choice strat1 strat2 predicate)
  (lambda (state)
    (if (predicate state)
        (strat1 state)
        (strat2 state))))

;;A strategy which does not burn until the ship is below 40km, then asks the user whether to burn or not.
(define (ask-after-40 ship-state)
  (choice ask-user no-burn (lambda (ship-state)
                             (if (< (ship-height ship-state) 40.0)
                                 #t
                                 #f))))

;square function
(define (square x) (* x x))
;A strategy which burns at a constant rate, just enough to land the ship with zero velocity at touchdown.
(define (constant-acc ship-state)
  (+ (/ (square (ship-velocity ship-state)) (* 2 (ship-height ship-state))) gravity)
  )

;A strategy that delays as long as possible, before burning at a constant rate.
(define (optimal-constant-acc ship-state)
  (define v (ship-velocity ship-state))
  (define h (ship-height ship-state))
  ; cut-h = (v^2 + h) / 2.0
  (define cut-h (/ (+ (* v v) h) 2.0))
  (if (< (ship-height ship-state) (- cut-h (* (* dt (ship-velocity ship-state)) 0.5)))
      (full-burn ship-state)
      (no-burn ship-state))
  )

;;Returns either strat1 or strat2, whichever is better.
(define (best-strategy strat1 strat2)
  (define (lander-loop2 state strat)
    (if (landed? state)
        ;if landed safely, should return the remaining fuel, or if not, should return an error code -100
        (if (landed-safely? state) (ship-fuel state) -100)
        ;if not landed yet, continue the process
        (lander-loop2 (update state (strat state)) strat)))
  (define s1 (lander-loop2 initial-ship strat1))
  (define s2 (lander-loop2 initial-ship strat2))
  ; condition 1: if one is crashed, it should give an error code, which is smaller than the remaining fuel of the other ship
  ; condition 2: if both landed safely, they all should return remaining fuel
  (if (> s1 s2) strat1 strat2)
  )







