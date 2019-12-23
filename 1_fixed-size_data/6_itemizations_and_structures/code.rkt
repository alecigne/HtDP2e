;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname code) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Space Invader
; Exercises 94 (cf. files/photo.jpg), 95,
; 96 (TODO), 97, 98

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Physical world

; Background

(define WORLD-WIDTH 400)
(define WORLD-HEIGHT 200)

; UFO

(define UFO-RADIUS 10)
(define UFO-WING-WIDTH (* UFO-RADIUS 4))
(define UFO-WING-HEIGHT (/ UFO-RADIUS 2.5))
(define UFO-COLOR "green")
(define UFO-SPEED 0.5)

; Tank

(define TANK-WIDTH 20)
(define TANK-HEIGHT 20)
(define TANK-COLOR "black")

; Missile

(define MISSILE-HEIGHT 10)
(define MISSILE-COLOR "red")
(define MISSILE-SPEED 4)

;; Graphical constants

; Background

(define SCENE (place-image/align (bitmap "files/background.jpeg")
                     0 0 "left" "top"
                     (empty-scene WORLD-WIDTH
                                  WORLD-HEIGHT)))
; UFO

(define UFO (overlay (circle UFO-RADIUS "solid" UFO-COLOR)
                     (rectangle UFO-WING-WIDTH UFO-WING-HEIGHT "solid" UFO-COLOR)))

; Tank

(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
(define TANK-YPOS (* WORLD-HEIGHT 8/10))

; Missile

(define MISSILE (triangle MISSILE-HEIGHT
                          "solid"
                          MISSILE-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A UFO is a Posn.
; Interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention).

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number)
; Interpretation: (make-tank x dx) specifies the position:
; (x, TANK-YPOS) and the tank's speed: dx pixels/tick.

; A Missile is a Posn.
; Interpretation: (make-posn x y) is the missile's place.

(define-struct aim [ufo tank])
; A Aim is a structure:
;   (make-aim UFO Tank)
; Interpretation: (make-aim u t) is the state of the game
; when a missile is not fired, including a UFO u and a tank t.

(define-struct fired [ufo tank missile])
; A Fired is a structure:
;   (make-fired UFO Tank Missile)
; Interpretation: (make-aim u t m) is the state of the game
; when a missile is fired, including a UFO u, a tank t and a
; missile m.

; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; Interpretation: represents the complete state of a
; space invader game (Space Invader Game State).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define UFO1 (make-posn 10 60))
(define TANK1 (make-tank 50 TANK-YPOS))
(define MISSILE1 (make-posn 20 80))

; Exercise 95
; Because the data definition includes two different structures
; corresponding to these two different situations.

; The tank maneuvers into position to fire the missile
(define SIGS1
  (make-aim (make-posn 200 50)
            (make-tank 200 -3)))

; Just like the previous one but the missile has been fired
(define SIGS2
  (make-fired (make-posn 200 75)
              (make-tank 200 -3)
              (make-posn 200 (- TANK-YPOS 20))))

; The missile is about to collide with the UFO
(define SIGS3
  (make-fired (make-posn 200 100)
              (make-tank 200 3)
              (make-posn 200 103)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rendering

; render: SIGS -> Image
; Adds TANK, UFO and possibily MISSILE to
; the background SCENE

(check-expect
 (si-render SIGS1)
 (tank-render (aim-tank SIGS1)
              (ufo-render (aim-ufo SIGS1)
                          SCENE)))

(check-expect
 (si-render SIGS3)
 (tank-render (fired-tank SIGS3)
              (missile-render (fired-missile SIGS3)
                              (ufo-render
                               (fired-ufo SIGS3)
                               SCENE))))

(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) SCENE))]
    [(fired? s)
     (tank-render
      (fired-tank s)
      (missile-render (fired-missile s)
                      (ufo-render (fired-ufo s)
                                  SCENE)))]))

; Exercise 97
; Same result when there is no overlap only.

; tank-render: Tank Image -> Image
; Adds the tank t to the given image im.

(check-expect (tank-render TANK1 SCENE)
              (place-image TANK
                           50 TANK-YPOS
                           SCENE))

(define (tank-render t im)
  (place-image TANK
               (tank-loc t) TANK-YPOS
               im))

; ufo-render: UFO Image -> Image
; Adds u to the given image im.

(check-expect (ufo-render UFO1 SCENE)
              (place-image UFO
                           10 60
                           SCENE))

(define (ufo-render u im)
  (place-image UFO
               (posn-x u) (posn-y u)
               im))

; missile-render: Missile Image -> Image
; Adds m to the given image im.

(check-expect (missile-render MISSILE1 SCENE)
              (place-image MISSILE
                           20 80
                           SCENE))

(define (missile-render m im)
  (place-image MISSILE
               (posn-x m) (posn-y m)
               im))

;; Moving the game elements

; si-move: SIGS -> SIGS
; Moves the objects of the game. This function calls a random
; action to move the UFO.

(define (si-move s)
  (si-move-proper s (random 50)))

; si-move-proper: SIGS -> SIGS
; Moves the objects of the game. This function uses a number to
; predictably move the UFO.

(define (si-move-proper s delta)
  (cond
    [(aim? s)
     (make-aim (update-ufo (aim-ufo s) delta)
               (update-tank (aim-tank s)))]
    [(fired? s)
     (make-fired (update-ufo (fired-ufo s) delta)
                 (update-tank (fired-tank s))
                 (update-missile (fired-missile s)))]))

; update-ufo: UFO -> UFO
; Updates the position of UFO u by UFO-SPEED. The jumps to
; the left or right are random.

;(check-expect (update-ufo (make-posn 200 50))
;              (make-posn 200 (+ 50 UFO-SPEED)))

(define (update-ufo u delta)
  (make-posn (+ (/ WORLD-WIDTH 2) delta)
             (+ (posn-y u) UFO-SPEED)))

; update-tank: Tank -> Tank
; Updates the position of Tank t according to its velocity.

(define (update-tank t)
  (make-tank (+ (tank-loc t)
                (tank-vel t))
             (tank-vel t)))

; update-missile: Missile -> Missile
; Updates the position of Missile m by MISSILE-SPEED

(define (update-missile m)
  (make-posn (posn-x m)
             (- (posn-y m) MISSILE-SPEED)))

;; Controls

; si-control: SIGS KeyEvent -> SIGS
; Moves the tank (left or right) or fire a missile.

(define (si-control s ke)
  (cond
    [(key=? ke "left") (tank-left s)]
    [(key=? ke "right") (tank-right s)]
    [(key=? ke " ") (fire-missile s)]
    [else s]))

; tank-left: SIGS -> SIGS
; Moves the tank to the left.

(define (tank-left s)
  (cond [(aim? s)
         (make-aim (aim-ufo s)
                   (tank-turn-left (aim-tank s)))]
        [(fired? s)
         (make-fired (fired-ufo s)
                     (tank-turn-left (fired-tank s))
                     (fired-missile s))]))

; tank-turn-left: Tank -> Tank
; Moves the tank to the left.

(define (tank-turn-left t)
  (cond [(<= 0 (tank-vel t))
         (make-tank (tank-loc t) (- 0 (tank-vel t)))]
        [else t]))

; tank-right: SIGS -> SIGS
; Moves the tank to the left.

(define (tank-right s)
  (cond [(aim? s)
         (make-aim (aim-ufo s)
                   (tank-turn-right (aim-tank s)))]
        [(fired? s)
         (make-fired (fired-ufo s)
                     (tank-turn-right (fired-tank s))
                     (fired-missile s))]))

; tank-turn-right: Tank -> Tank
; Moves the tank to the right.

(define (tank-turn-right t)
  (cond [(< (tank-vel t) 0)
         (make-tank (tank-loc t) (- 0 (tank-vel t)))]
        [else t]))

(define (fire-missile s)
  (cond [(aim? s)
         (make-fired (aim-ufo s)
                     (aim-tank s)
                     (make-posn (tank-loc (aim-tank s))
                                (- TANK-YPOS (/ TANK-HEIGHT 2))))]
        [(fired? s)
         (make-fired (fired-ufo s)
                     (fired-tank s)
                     (make-posn (tank-loc (fired-tank s))
                                (- TANK-YPOS (/ TANK-HEIGHT 2))))]))


;; Ending

; si-game-over?: SIGS -> Boolean
; Ends the game when the UFO lands or when the missile
; hits the UFO.

(check-expect (si-game-over? SIGS1)
              #false)

(check-expect (si-game-over? SIGS1)
              #false)

(check-expect (si-game-over? SIGS3)
              #true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (ufo-landed? (aim-ufo s))]
    [(fired? s)
     (or
      (ufo-landed? (fired-ufo s))
      (ufo-hit? (fired-ufo s)
                (fired-missile s)))]))

; ufo-landed?: UFO -> Boolean
; Checks if a UFO u has landed.

(check-expect (ufo-landed? (make-posn 50 (- WORLD-HEIGHT UFO-RADIUS)))
              #true)

(define (ufo-landed? u)
  (>= (posn-y u) (- WORLD-HEIGHT UFO-RADIUS)))

; ufo-hit?: UFO Missile -> Boolean
; Checks if a missile m is close to a UFO u.

(check-expect (ufo-hit? (make-posn 100 100)
                        (make-posn 102 99))
              #true)

(define (ufo-hit? u m)
  (and (<= (abs (- (posn-x u)
                   (posn-x m))) UFO-RADIUS)
       (<= (abs (- (posn-y u)
                   (posn-y m))) UFO-RADIUS)))

; si-render-final: Image
; Renders the final step of the game.

(define (si-render-final s)
  (cond
    [(and (aim? s)
          (ufo-landed? (aim-ufo s)))
     (game-over "Game over!")]
    [(and (fired? s)
          (ufo-landed? (fired-ufo s)))
     (game-over "Game over!")]
    [(and (fired? s)
         (ufo-hit? (fired-ufo s)
                   (fired-missile s)))
     (game-over "You win!")]))

(define (game-over txt)
  (place-image (text txt 50 "red")
               (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)
               SCENE))

;; Main

(define (main s)
  (big-bang s
            [to-draw si-render]
            [stop-when si-game-over? si-render-final]
            [on-key si-control]
            [on-tick si-move]))

(main (make-aim (make-posn (/ WORLD-WIDTH 2) 0)
                (make-tank (/ WORLD-WIDTH 2) 4)))