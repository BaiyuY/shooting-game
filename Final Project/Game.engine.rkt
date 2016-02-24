;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Game engine|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "struct-inheritance.rkt")

;;;
;;; EECS-111 A simple imperative game engine
;;; using big-bang and OOP
;;;

;;;
;;; Turnable constants
;;;

(define window-width 800)
(define window-height 600)
(define frame-rate 30)
(define inter-frame-interval (/ 1.0 frame-rate))

;;;
;;; Type definitions
;;;

(define-struct game-object
  ([position #:mutable]
   [velocity #:mutable]
   [orientation #:mutable]
   [rotational-velocity #:mutable])
  #:methods
  (define (update! object)
    ;; Do nothing
    (void)))

;;;
;;; Tracking game objects
;;;

(define all-game-objects '())

(define (destroy! object)
  (set! all-game-objects
        (remove object all-game-objects)))

(define (add-game-object! object)
  (cons object all-game-objects))

;;;
;;; Event dispatch
;;;

(define (on-key-press key)
  ;; Fill this in with your key bindings
  (cond ;[(equal? key "up")
        ; (set! firing-engines? true)]
        ;[(equal? key "left")
        ; (set-game-object-rotational-velocity! the-player
        ;                                       -2)]
        ;[(equal? key "right")
        ; (set-game-object-rotational-velocity! the-player
        ;                                       2)]
        ;[(equal? key " ")
        ; (fire-missile!)]
        [else null]))

(define (on-key-release key)
  ;; Fill this in with your key bindings
  (cond ;[(equal? key "up")
        ; (set! firing-engines? false)]
        ;[(or (equal? key "left")
        ;     (equal? key "right"))
        ; (set-game-object-rotational-velocity! the-player 0)]
        [else null]))

;;;
;;; Driver loop
;;;

(define (game objects)
  (begin (set! all-game-objects objects)
         (big-bang all-game-objects
                   (on-key (λ (ignore key)
                             (begin (on-key-press key)
                                    all-game-objects)))
                   (on-release (λ (ignore key)
                                 (begin (on-key-release key)
                                        all-game-objects)))
                   (on-tick (lambda (game-objects)
                              (begin (for-each update! game-objects)
                                     (update-physics!)
                                     all-game-objects))
                            inter-frame-interval)
                   (to-draw (lambda (game-objects)
                              (foldl (lambda (object scene)
                                       (place-image (rotate (radians->rotation (game-object-orientation object))
                                                            (render object))
                                                    (posn-x (game-object-position object))
                                                    (posn-y (game-object-position object))
                                                    scene))
                                     (rectangle window-width window-height "solid" "black")
                                     game-objects))
                            window-width
                            window-height))))

(define radians->rotation-coefficient
  (/ 360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     90))

;;;
;;; State update
;;;

(define (update-physics!)
  (begin (for-each (λ (object)
                     (begin (set-game-object-orientation! object
                                                          (+ (game-object-orientation object)
                                                             (* inter-frame-interval
                                                                (game-object-rotational-velocity object))))
                            (set-game-object-position! object
                                                       (local [(define new-position
                                                                 (posn-+ (posn-* inter-frame-interval
                                                                                 (game-object-velocity object))
                                                                         (game-object-position object)))]
                                                         (make-posn (wrap (posn-x new-position) window-width)
                                                                    (wrap (posn-y new-position) window-height))))))
                   all-game-objects)
         (handle-collisions all-game-objects)))

;;;
;;; Collision handling
;;;

(define (handle-collisions objects)
  (unless (empty? objects)
    (local [(define head (first objects))
            (define tail (rest objects))]
      (begin (for-each (λ (object)
                         (when (collided? head object)
                           (handle-collision head object)))
                       tail)
             (handle-collisions tail)))))

(define (collided? a b)
  (< (distance-squared (game-object-position a)
                       (game-object-position b))
     (squared (+ (radius a)
                 (radius b)))))

(define (handle-collision a b)
  (if (and (asteroid? a)
           (asteroid? b))
      (bounce a b)
      (begin (destroy! a)
             (destroy! b))))

(define (bounce a b)
  (local [(define mass-a (mass a))
          (define mass-b (mass b))
          (define vel-a (game-object-velocity a))
          (define vel-b (game-object-velocity b))
          (define one-over-mass (/ 1 (+ mass-a mass-b)))]
    (begin (set-game-object-velocity! a
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-a mass-b)
                                                              vel-a)
                                                      (posn-* (* 2 mass-b)
                                                              vel-b))))
           (set-game-object-velocity! b
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-b mass-a)
                                                              vel-b)
                                                      (posn-* (* 2 mass-a)
                                                              vel-a)))))))

(define (mass asteroid)
  (squared (radius asteroid)))

;;;
;;; Vector arithmetic
;;;

(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

(define (posn-- a b)
  (make-posn (- (posn-x a)
                (posn-x b))
             (- (posn-y a)
                (posn-y b))))

(define (heading-of object viewer)
  (unit-vector (posn-- (game-object-position object)
                       (game-object-position viewer))))

(define (unit-vector posn)
  (posn-* (/ 1 (posn-magnitude posn))
          posn))

(define (posn-magnitude posn)
  (sqrt (+ (squared (posn-x posn))
           (squared (posn-y posn)))))

(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))

(define (forward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

(define (closest-asteroid-to game-object)
  (arg-min (lambda (x)
             (distance-squared (game-object-position game-object)
                               (game-object-position x)))
           (filter asteroid?
                   all-game-objects)))

(define (arg-min f list)
  (local [(define (loop best best-score remaining)
            (if (empty? remaining)
                best
                (local [(define score (f (first remaining)))]
                  (if (< score best-score)
                      (loop (first remaining)
                            score
                            (rest remaining))
                      (loop best best-score
                            (rest remaining))))))]
    (loop (first list)
          (f (first list))
          (rest list))))

;;;
;;; Randomization
;;;

(define random-color
  (local [(define colors
            (list (color 255 0 0)
                  (color 0 255 0)
                  (color 0 0 255)
                  (color 128 128 0)
                  (color 128 0 129)
                  (color 0 128 128)))]
    (λ () (random-element colors))))

(define (random-element list)
  (list-ref list
            (random (length list))))

(define (random-float min max)
  (+ min
     (* (random)
        (- max min))))

(define (random-velocity)
  (make-posn (random-float -10 10)
             (random-float -10 10)))

;;;
;;; Other arithmetic utilities
;;;

(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))

