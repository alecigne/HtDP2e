;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 7_happy_cat_ex91) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; The happy cat that turns around
; Exercise 91

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World

(define WORLD-WIDTH 400)
(define WORLD-HEIGHT 200)
(define WORLD (empty-scene WORLD-WIDTH
                           WORLD-HEIGHT))

;; Cat

(define CAT (bitmap "files/cat1.png"))
(define CAT2 (bitmap "files/cat2.png"))
(define CAT-HEIGHT (image-height CAT))
(define CAT-WIDTH (image-width CAT))
(define CAT-CENTER (/ CAT-WIDTH 2))
(define CAT-SPEED 3) ; pixels per clock tick

;; Gauge

(define GAUGE-HEIGHT (/ WORLD-HEIGHT 20))
(define GAUGE-FROM-TOP (/ WORLD-HEIGHT 10))
(define GAUGE-SPEED 1) ; pixels per clock tick

;; Cat management

(define PET-INDEX 5)
(define FEED-INDEX 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Happiness is a Number that falls into [0,100].
; Interpretation: happiness of the cat.

; ensure-hn: Number -> Happiness
; Ensures that happiness hn falls into 0 and 100.

(check-expect (ensure-hn -10) 0)
(check-expect (ensure-hn 127) 100)
(check-expect (ensure-hn 2) 2)

(define (ensure-hn hn)
  (cond
    [(< hn 0) 0]
    [(> hn 100) 100]
    [else hn]))

; A Direction is a Boolean
; Interpretation: the direction of the cat's movement. #true is right,
; #false is left.

(define-struct cat [x happiness direction])
; A VCat is a structure:
;  (make-cat Number Happiness Direction)
; Interpretation: (make-cat x h d) represents the cat's position x
; on the x-axis (relative to the cat center), the cat's level of
; happiness the direction of its movement.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rendering

; draw-gauge: Happiness -> Image
; Draws a gauge whose width depends on happiness hn.

(check-expect (draw-gauge 0)
              (rectangle 0 GAUGE-HEIGHT
                         "solid" "red"))

(check-expect (draw-gauge 100)
              (rectangle WORLD-WIDTH GAUGE-HEIGHT
                         "solid" "green"))

(check-expect (draw-gauge 56)
              (rectangle (* WORLD-WIDTH (/ 56 100)) GAUGE-HEIGHT
                         "solid" "yellow"))

(check-expect (draw-gauge 75)
              (rectangle (* WORLD-WIDTH (/ 75 100)) GAUGE-HEIGHT
                         "solid" "green"))

(define (draw-gauge hn)
  (rectangle (* WORLD-WIDTH (/ hn 100)) GAUGE-HEIGHT
             "solid" (cond
                       [(<= 75 hn 100) "green"]
                       [(<= 25 hn 74) "yellow"]
                       [(<= 0 hn 24) "red"])))

; draw-cat: Number -> Image
; Returns a cat whose apparence depends on its position x.

(check-expect (draw-cat 15) CAT)
(check-expect (draw-cat 20) CAT2)

(define (draw-cat x)
  (cond
    [(odd? (floor (/ x 10))) CAT]
    [else CAT2]))

; render: VCat -> Image
; Renders the cat and its level of happiness from
; the VCat structure vcat.

(check-expect
 (render (make-cat 0 100 #true))
 (place-image/align (draw-gauge 100)
                    0 GAUGE-FROM-TOP "left" "bottom"
                    (place-image (draw-cat 0)
                                 0 (- WORLD-HEIGHT (/ CAT-HEIGHT 2))
                                 WORLD)))

(define (render vc)
  (place-image/align (draw-gauge (cat-happiness vc))
                     0 GAUGE-FROM-TOP "left" "bottom"
                     (place-image (draw-cat (cat-x vc))
                                  (cat-x vc) (- WORLD-HEIGHT (/ CAT-HEIGHT 2))
                                  WORLD)))

;; Clock-tick-handler

; update-x: Number Direction -> Number
; Updates the x-position x of the cat according to its direction dir.

(check-expect (update-x 0 #true)
              CAT-SPEED)

(check-expect (update-x 80 #false)
              (- 80 CAT-SPEED))

(define (update-x x dir)
  (cond
    [dir (+ x CAT-SPEED)]
    [(not dir) (- x CAT-SPEED)]))

; update-hn: Happiness -> Happiness
; Reduces the cat's happiness hn by GAUGE-SPEED.

; /!\ A Happiness is returned and must falls into [0,100].

(check-expect (update-hn 0) 0)

(check-expect (update-hn 0.5) 0)

(check-expect (update-hn 50)
              (- 50 GAUGE-SPEED))

(define (update-hn hn)
  (ensure-hn (- hn GAUGE-SPEED)))

; update-dir: Direction -> Direction
; Changes the direction of the cat if he or she reaches the border.

(check-expect (update-dir (- WORLD-WIDTH CAT-SPEED) #true)
              #false)

(check-expect (update-dir (- WORLD-WIDTH CAT-SPEED) #false)
              #false)

(check-expect (update-dir CAT-SPEED #true)
              #true)

(check-expect (update-dir CAT-SPEED #false)
              #true)

(define (update-dir x dir)
  (cond
    [(and (>= x (- WORLD-WIDTH CAT-SPEED))
          dir)
     #false]
    [(and (<= x CAT-SPEED)
          (not dir))
     #true]
    [else dir]))

; update-cat: VCat -> VCat
; Moves the VCat by 3 pixels modulo WORLD-WIDTH,
; and decreases the cat's happiness by GAUGE-SPEED with
; every clock tick.

(check-expect (update-cat (make-cat 50 50 #true))
              (make-cat 53 49 #true))

(check-expect (update-cat (make-cat 50 50 #false))
              (make-cat 47 49 #true))

(define (update-cat vc)
  (make-cat (update-x (cat-x vc) (cat-direction vc))
            (update-hn (cat-happiness vc))
            (update-dir (cat-x vc) (cat-direction vc))))

;; Key event handler

; pet: VCat -> VCat
; Increases happiness in VCat vcat by PET-INDEX.

; /!\ A Happiness is returned and must falls into [0,100].

(check-expect (pet (make-cat 0 (+ (- 100 PET-INDEX) 1) "left"))
              (make-cat 0 100 "left"))
(check-expect (pet (make-cat 27 100 "left"))
              (make-cat 27 100 "left"))
(check-expect (pet (make-cat 99 50 "left"))
              (make-cat 99 (+ 50 PET-INDEX) "left"))

(define (pet vc)
  (make-cat (cat-x vc)
            (ensure-hn (+ (cat-happiness vc) PET-INDEX))
            (cat-direction vc)))

; feed: VCat -> VCat
; Increases happiness in VCat vcat by FEED-INDEX.

; /!\ A Happiness is returned and must falls into [0,100].

(check-expect (feed (make-cat 50 10 "left"))
              (make-cat 50 (+ 10 FEED-INDEX) "left"))

(define (feed vc)
  (make-cat (cat-x vc)
            (ensure-hn (+ (cat-happiness vc) FEED-INDEX))
            (cat-direction vc)))

; ke-h: VCat KeyEvent -> VCat
; Increases happiness by 1/5 or 1/3 in VCat vcat when pressing
; the down or up arrow key, respectively.

(check-expect (ke-h (make-cat 57 10 "left") "down")
              (pet (make-cat 57 10 "left")))

(define (ke-h vc ke)
  (cond
    [(key=? ke "down") (pet vc)]
    [(key=? ke "up") (feed vc)]
    [else vc]))

;; End

; end?: VCat -> Boolean
; Returns #true when cat's happiness reaches 0.

(define (end? vc)
  (<= (cat-happiness vc) 0))

; main: WorldState -> WorldState
; Launches the program from some initial state.

(define (main vc)
  (big-bang vc
            [to-draw render]
            [on-tick update-cat]
            [on-key ke-h]
            [stop-when end?]
            ))

