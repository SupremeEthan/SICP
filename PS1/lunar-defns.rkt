#lang racket

;; This line tells Racket to let other files import any definitions in this
;; file; it's a bit like making everything `public` in Java.
(provide (all-defined-out))

;; First, we define a struct for the ship's state; a struct is like a class
;; without any methods, containing only the instance variables.
;; For more details, see https://docs.racket-lang.org/guide/define-struct.html
(struct ship
  (height   ; the ship's current height, in kilometers.
   velocity ; the ship's current velocity, in kilometers per second.
   fuel))   ; how much fuel the ship currently has left, in no particular unit.

;; Return a default starting state for the simulation.
(define initial-ship
  (ship 50.0 0.0 20.0))

;; Constants used in the simulation.

;; How many seconds each step simulates. The smaller this is, the more accurate
;; the simulation will be -- but the slower the program will take to run.
;; Units: seconds.
(define dt 0.5)

;; Acceleration due to gravity. Note that this is *not* the moon's actual
;; gravity.
;; Units: meters / second^2
(define gravity 0.5)

;; The safe landing velocity; landing the ship at velocities slower than this is
;; considered a safe landing, while landing the ship at velocities faster than
;; this will lead to rapid unplanned dissasembly.
;; See also: https://youtu.be/bvim4rsNHkQ?t=36
;; Units: meters / second
(define safe-velocity -0.5)

;; The engine's applied force.
;; Units: newton-seconds
(define engine-strength 1.0)  ; 1 kilonewton-second

;; Creates a new ship state one simulation-step after the current state.
;;
;; Arguments:
;;   state (ship?): The ship's current state.
;;   burn-rate (number?): The rate at which to burn fuel, from 0 to 1.
;;     A burn rate of 0 means to not fire the thrusters at all, a burn rate of
;;     0.5 means to fire the thrusters at half-strength, and a burn rate of 1
;;     means to fire the thrusters at full strength, their maximum power.
;;
;; Returns (ship?): The new ship state after running one step of the simulation.
;;   Note that the input argument `state` isn't modified at all.
(define (update state burn-rate)
  (ship ; Create a new ship state
   ; Height changes proportionally to the ship's velocity.
   ; In Java, this looks like `height += velocity * dt`.
   (+ (ship-height state) (* (ship-velocity state) dt))
   ; Velocity changes proportionally to the burn rate and gravity.
   ; velocity += dt * ((burn-rate * engine-strength) - gravity)
   (+ (ship-velocity state)
      (* (- (* engine-strength burn-rate) gravity)
         dt))
   ; fuel -= burn-rate * dt
   (- (ship-fuel state) (* burn-rate dt))))

;; A function which plays a game of lunar lander using a given burn-strategy,
;; printing the ship's state each step, and continuing until the rocket lands --
;; succesfully or otherwise.
;;
;; Arguments:
;;  state (ship?): The ship's current state.
;;  burn-strategy (-> ship? number?): A function which takes a ship-state and
;;    returns a burn-rate.
;;    See: Documentation for `update`.
;;
;; Returns (void?): Void.
(define (lander-loop state burn-strategy)
  (show-ship state)
  (if (landed? state)
      (end-game state)
      (lander-loop (update state (burn-strategy state)) burn-strategy)))

;; Has the ship landed?
;;
;; Arguments:
;;   state (ship?): A ship state.
;;
;; Returns (bool?): #t if the ship has landed, #f otherwise.
(define (landed? state)
  (<= (ship-height state) 0.0))

;; Has the ship landed safely?
;;
;; Arguments:
;;   state (ship?): A ship state.
;;
;; Returns (bool?): #t if the ship has landed with a velocity at or below the
;;   `safe-velocity`, #f otherwise.
(define (landed-safely? state)
  (and (landed? state)
       (>= (ship-velocity state) safe-velocity)))

;; Interactive function that asks the user to burn the rockets; helpful when
;; implementing an interactive burn-strategy.
;;
;; Returns (bool?): #t if the user answers "yes", #f if the user answers "no".
(define (ask-to-burn)
  (printf "Burn rockets? [y/n]: ")
  (let loop ((response (read-line)))
    (cond
      ((equal? response "y") #t)
      ((equal? response "n") #f)
      (else
       (printf "Please answer y or n.")
       (loop (read-line))))))

;; Prints the ship's state to the console.
;;
;; Arguments:
;;   state (ship?): A ship state.
;;
;; Returns (void?): Void.
(define (show-ship state)
  (printf "Height: ~vkm; Velocity: ~vkm/s; Fuel Remaining: ~v\n"
          (ship-height state)
          (ship-velocity state)
          (ship-fuel state)))

;; Prints a message at game-end.
;;
;; Arguments:
;;   final-state (ship?): The ship's final state at landing.
;;
;; Returns (void?): Void.
(define (end-game final-state)
  (printf "Final velocity: ~v\n" (ship-velocity final-state))
  (if (landed-safely? final-state)
      (displayln "Good landing!")
      (displayln "You crashed!")))
