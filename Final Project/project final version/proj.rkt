;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "struct-inheritance.rkt")


;; importing image

(define rockpic (bitmap "ian hair.png"))
(define temp (bitmap "wartank.png"))
(define tech (bitmap "techbg.jpg"))
(define bullet (bitmap "bullet.png"))


(define window-width 800)
(define window-height 600)
(define frame-rate 30)
(define inter-frame-interval (/ 1.0 frame-rate))



;; bounding box for hit detection
(define-struct bounding-box (x1 y1 x2 y2))

;;define game-object with method (get-bounding-box-self)
(define-struct game-object
  ([position #:mutable]
   [velocity #:mutable]
   [orientation #:mutable]
   [rotational-velocity #:mutable]
   [height #:mutable]
   [width #:mutable])
  #:methods
  (define (update! object)
    ;; Do nothing
    (void))
  (define (get-bounding-box self)
    (make-bounding-box
     (- (posn-x (game-object-position self)) (/ (game-object-width self) 2))
     (- (posn-y (game-object-position self)) (/ (game-object-height self) 2))
     (+ (posn-x (game-object-position self)) (/ (game-object-width self) 2))
     (+ (posn-y (game-object-position self)) (/ (game-object-height self) 2))                  
     ))
  )

;; added back? to allow player to move backward
(define firing-engines? false)
(define back? false)
;; added backward-direction so that we can implement moving backward easily
(define (backward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn  (-( cos o))
               (-( sin o)))))

(define-struct (player game-object)
  ()
  #:methods
  (define (update! player)
    (cond
      [(equal? firing-engines? true)
      (set-game-object-velocity! player
                                 (posn-* 80 (forward-direction the-player)))]
      
      [(equal? back? true)
       (set-game-object-velocity! player
                                 (posn-* 60 (backward-direction the-player)))]
      [(equal? back? false)
       (set-game-object-velocity! player
                                 (posn-* 0 (backward-direction the-player)))]
      [else null]
    ))
  
  (define (render player)
    (scale 0.3 temp))
   )
   

(define firing-engines2? false)
(define back2? false)




; define the second player
(define-struct (player2 game-object)
  ()
  #:methods
  (define (update! player2)
    (cond
      [(equal? firing-engines2? true)
      (set-game-object-velocity! player2
                                 (posn-* 80 (forward-direction the-player2)))]
      
      [(equal? back2? true)
       (set-game-object-velocity! player2
                                 (posn-* 60 (backward-direction player2)))]
      [(equal? back2? false)
       (set-game-object-velocity! player2
                                 (posn-* 0 (backward-direction the-player2)))]
      [else null]
    ))
  
  (define (render player2)
    (scale 0.3  temp)) 
   
  )

;define the wall object...eventhough its called wall , its actually the unpassable flame in the game...
(define-struct (wall game-object)
  
  ()
  #:methods
  (define (render wall)
    (rectangle 
               (- (bounding-box-y2 (get-bounding-box wall)) (bounding-box-y1 (get-bounding-box wall)))
               (- (bounding-box-x2 (get-bounding-box wall)) (bounding-box-x1 (get-bounding-box wall)))
             "outline" "transparent")))

;define rock object
(define-struct (rock game-object)
  
  ()
  #:methods
 
  (define (render rock )
    (scale 0.25 rockpic)))

  

;define the missile object
(define-struct (missile game-object)
  ([lifetime #:mutable])
  #:methods
  (define (update! m)
    (begin (set-missile-lifetime! m
                                  (- (missile-lifetime m)
                                     1))
           (when (zero? (missile-lifetime m))
             (destroy! m))))
  
  (define (render missile)
    (scale 0.03 bullet))
   
  (define (radius missile)
   3))



;; define what will happend when key pressed

(define (on-key-press key)
  (cond [(equal? key "i")
         (set! firing-engines? true)]
        [(equal? key "j")
         (set-game-object-rotational-velocity! the-player
                                               -3)]
        [(equal? key "l")
         (set-game-object-rotational-velocity! the-player
                                               3)]
        [(equal? key "k")
         (set! back? true)] 
        [(equal? key "w")
         (set! firing-engines2? true)]
      
        [(equal? key "a")
         (set-game-object-rotational-velocity! the-player2
                                               -3)]
        [(equal? key "d")
         (set-game-object-rotational-velocity! the-player2
                                               3)]
        [(equal? key "s")
         (set! back2? true)]
        [(equal? key ";")
         (fire-missile!)]
        [(equal? key "f")
         (fire-missile2!)]
        [else null]))

(define (on-key-release key)
  (cond [(equal? key "i")
         (set! firing-engines? false)]
        [(equal? key "k")
         (set! back? false)]
        [(or (equal? key "j")
             (equal? key "l"))
         (set-game-object-rotational-velocity! the-player 0)]
        [(equal? key "w")
         (set! firing-engines2? false)]
        [(equal? key "s")
         (set! back2? false)]
        [(or (equal? key "a")
             (equal? key "d"))
         (set-game-object-rotational-velocity! the-player2 0)]
        [else null]))

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


; function that detects hitting
(define (collided? 1-b 2-b)
  (or (and
       (or (within? (bounding-box-x1 (get-bounding-box 1-b)) (bounding-box-x1 (get-bounding-box 2-b)) (bounding-box-x2 (get-bounding-box 2-b)))
           (within? (bounding-box-x2 (get-bounding-box 1-b)) (bounding-box-x1 (get-bounding-box 2-b)) (bounding-box-x2 (get-bounding-box 2-b))))
       (or (within? (bounding-box-y1 (get-bounding-box 1-b)) (bounding-box-y1 (get-bounding-box 2-b)) (bounding-box-y2 (get-bounding-box 2-b)))
           (within? (bounding-box-y2 (get-bounding-box 1-b)) (bounding-box-y1 (get-bounding-box 2-b)) (bounding-box-y2 (get-bounding-box 2-b)))))
      (and
       (or (within? (bounding-box-x1 (get-bounding-box 2-b)) (bounding-box-x1 (get-bounding-box 1-b)) (bounding-box-x2 (get-bounding-box 1-b)))
           (within? (bounding-box-x2 (get-bounding-box 2-b)) (bounding-box-x1 (get-bounding-box 1-b)) (bounding-box-x2 (get-bounding-box 1-b))))
       (or (within? (bounding-box-y1 (get-bounding-box 2-b)) (bounding-box-y1 (get-bounding-box 1-b)) (bounding-box-y2 (get-bounding-box 1-b)))
           (within? (bounding-box-y2 (get-bounding-box 2-b)) (bounding-box-y1 (get-bounding-box 1-b)) (bounding-box-y2 (get-bounding-box 1-b)))))
      ))
(define (within? a b c)
  (<= b a c))

; fucntion that defines what will happen when different objects hit each other
(define (handle-collision a b)
(begin
  (cond
    [(or (and ( equal? a the-player ) (equal? b the-player2 )) (and ( equal? b the-player ) (equal? a the-player2 )))
    (begin (destroy! a) (destroy! b))]
    ;  wall & players
    [(and (equal? a the-player) (wall? b ) )
      (destroy! a)  ]
    [(and (equal? b the-player) (wall? a ) )
      (destroy! b)  ]
    [(and (equal? a the-player2) (wall? b ) )
      (destroy! a)  ]
    [(and (equal? b the-player2) (wall? a ) ) 
      (destroy! b)  ]
    ; rock & wall

    [(and (wall? a) (rock? b))
    (set-game-object-velocity! b (posn-* -1 (game-object-velocity b) )) ]
    [(and (wall? b) (rock? a))
    (set-game-object-velocity! a (posn-* -1 (game-object-velocity a) )) ]
    ; rock & tank
    
    [(and (equal? a the-player) (rock? b ) )
      (destroy! a)  ]
    [(and (equal? b the-player) (rock? a ) )
      (destroy! b)  ]
    [(and (equal? a the-player2) (rock? b ) )
      (destroy! a)  ]
    [(and (equal? b the-player2) (rock? a ) ) 
      (destroy! b)  ]
    ;missile & tank
    
    [(and (equal? a the-player) (missile? b ) )
     (begin (destroy! a) (destroy! b) )]
    [(and (equal? b the-player) (missile? a ) )
      (begin (destroy! a) (destroy! b) )]
    [(and (equal? a the-player2) (missile? b ) )
      (begin (destroy! a) (destroy! b) )]
    [(and (equal? b the-player2) (missile? a ) ) 
      (begin (destroy! a) (destroy! b) )]

    ;missile & wall
    [(and (missile? a) (wall? b ) )
     (destroy! a)  ]
    [(and (missile? b) (wall? a ) )
     (destroy! b)  ]

    ;missile & rock
    [(and (missile? a) (rock? b ) )
     (destroy! a)  ]
    [(and (missile? b) (rock? a ) )
     (destroy! b)  ]
     
    [else void]
       )
       ))


;;; Tracking game objects


(define all-game-objects '())

(define (destroy! object)
  (set! all-game-objects
        (remove object all-game-objects)))

(define the-player '())
(define the-player2 '())
(define the-wall '())
(define the-rock '())
(define the-rock2 '())
(define the-rock3 '())
(define the-wall2 '())
(define the-wall3 '())
(define the-wall4 '())



;;; Object creation


;; have to fire-missile for 2 different players
(define (fire-missile!)
  (local [(define forward (forward-direction the-player))]
    (set! all-game-objects
          (cons (make-missile (posn-+ (game-object-position the-player)
                                      (posn-* (+(/ (game-object-height the-player) 2)3)
                                              forward))
                              (posn-+ (game-object-velocity the-player)
                                      (posn-* 200 forward))
                              0
                              0
                              1.5
                              1.5
                              100)
                all-game-objects))))

(define (fire-missile2!)
  (local [(define forward (forward-direction the-player2))]
    (set! all-game-objects
          (cons (make-missile (posn-+ (game-object-position the-player2)
                                      (posn-* (+(/ (game-object-height the-player2) 2)3)
                                              forward))
                              (posn-+ (game-object-velocity the-player2)
                                      (posn-* 200 forward))
                              0
                              0
                              1.5
                              1.5
                              100)
                all-game-objects))))

;;; Driver loop


(define (tanks)
  (begin (set! the-player
               (make-player (make-posn 30
                                       575)
                            (make-posn 0 0)
                            0
                            0
                            30
                            30))
         (set! the-player2
               (make-player2 (make-posn  775
                                         30 )
                            (make-posn 0 0)
                            0
                            0
                            30
                            30))
         (set! the-wall
               (make-wall (make-posn 150
                                     225)
                            (make-posn 0 0)
                            0
                            0
                            50
                            300))
         (set! the-wall2
               (make-wall (make-posn 600
                                     115)
                            (make-posn 0 0)
                            0
                            0
                            230
                            100))
         (set! the-wall3
               (make-wall (make-posn 250
                                     525)
                            (make-posn 0 0)
                            0
                            0
                            150
                            40))
         (set! the-wall4
               (make-wall (make-posn 600
                                     375)
                            (make-posn 0 0)
                            0
                            0
                            50
                            400))
         (set! the-rock
               (make-rock (make-posn 150 10)
                           (make-posn 0 100)
                            0
                            0
                            27
                            27))
         (set! the-rock2
               (make-rock (make-posn 785 520)
                           (make-posn -100 0)
                            0
                            0
                            27
                            27))
         (set! the-rock3
               (make-rock (make-posn 450 5)
                           (make-posn 0 100)
                            0
                            0
                            27
                            27))
         
         (set! all-game-objects
              (list the-rock  the-wall  the-player  the-player2 the-wall2 the-wall3 the-wall4 the-rock2 the-rock3))
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
                                     tech
                                     all-game-objects))
                            800
                            600))))

; below are code that we didn't dare to touch


(define radians->rotation-coefficient
  (/ -360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     -90))





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
    (make-posn  ( cos o)
               ( sin o))))



(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else 
         number]))

(define (squared x)
  (* x x))



(tanks)
