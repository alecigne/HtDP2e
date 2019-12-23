;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8_cham_ex92) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Cham
; Exercise 92

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameters of the world

; World

(define WORLD-WIDTH 400)
(define WORLD-HEIGHT 200)

; Happiness

(define UNHAPPINESS-SPEED 0.01)
(define FEED-INDEX 0.1)

; Speed

(define CHAM-SPEED 3)

;; Graphical constants

; World

(define WORLD (empty-scene WORLD-WIDTH WORLD-HEIGHT))

; Gauge

(define GAUGE-HEIGHT (/ WORLD-HEIGHT 20))
(define GAUGE-FROM-TOP (/ WORLD-HEIGHT 10))

; Chameleon

(define CHAM (bitmap "files/cham2.png"))
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-HEIGHT (image-height CHAM))
(define CHAM-VCENTER (/ CHAM-HEIGHT 2))
(define CHAM-HCENTER (/ CHAM-WIDTH 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Happiness is a Number that falls into [0, 1]
; Interpretation: happiness of the chameleon.

; ensure-hn: Number -> Happiness
; Converts a Number n to a Happiness.

(check-expect
 (ensure-hn -0.5)
 0)

(check-expect
 (ensure-hn 1.1)
 1)

(check-expect
 (ensure-hn 0.7)
 0.7)

(define (ensure-hn n)
  (cond
    [(< n 0) 0]
    [(> n 1) 1]
    [else n]))

; A Color is one of:
; - "red"
; - "blue"
; - "green"
; Interpretation: the color of the chameleon.

; A KeyEvent is one of:
; - "r"
; - "b"
; - "g"
; - "down"
; Interpretation: a keyboard key.

(define-struct cham [x hap col])
; A VCham is a structure:
;  (make-cham Number Happiness Color)
; Interpretation: (make-cham x h c) is a chameleon in x-position
; x relative to the right of the chameleon, a happiness state of
; h and whose color is c.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rendering

; draw-gauge: Happiness -> Image
; Draws a gauge from the happiness hn of the chameleon.

(check-expect
 (draw-gauge 1)
 (rectangle WORLD-WIDTH
            GAUGE-HEIGHT
            "solid" "green"))

(check-expect
 (draw-gauge 0.41)
 (rectangle (* WORLD-WIDTH 0.41)
            GAUGE-HEIGHT
            "solid" "yellow"))

(check-expect
 (draw-gauge 0.1)
 (rectangle (* WORLD-WIDTH 0.1)
            GAUGE-HEIGHT
            "solid" "red"))

(define (draw-gauge hn)
  (rectangle (* hn WORLD-WIDTH)
             GAUGE-HEIGHT
             "solid" (cond
                       [(<= 0.75 hn 1) "green"]
                       [(<= 0.25 hn 0.74) "yellow"]
                       [(<= 0 hn 0.24) "red"])))

; draw-cham: Color -> Image
; Draws a chameleon with the right color col.

(check-expect
 (draw-cham "red")
 (overlay CHAM
          (rectangle CHAM-WIDTH CHAM-HEIGHT
                     "solid" "red")))

(check-expect
 (draw-cham "green")
 (overlay CHAM
          (rectangle CHAM-WIDTH CHAM-HEIGHT
                     "solid" "green")))

(check-expect
 (draw-cham "blue")
 (overlay CHAM
          (rectangle CHAM-WIDTH CHAM-HEIGHT
                     "solid" "blue")))

(define (draw-cham col)
  (overlay CHAM
           (rectangle CHAM-WIDTH CHAM-HEIGHT
                      "solid" col)))
                         
; render: VCham -> Image
; Draws the world from a VCham vc, including the happiness gauge.

(check-expect
 (render (make-cham 0 1 "red"))
 (place-image/align (draw-gauge 1)
                    0 GAUGE-FROM-TOP "left" "top"
                    (place-image (draw-cham "red")
                                 (- 0 CHAM-HCENTER)
                                 (sub1 (- WORLD-HEIGHT CHAM-VCENTER))
                                 WORLD)))

(check-expect
 (render (make-cham 50 0.5 "blue"))
 (place-image/align (draw-gauge 0.5)
                    0 GAUGE-FROM-TOP "left" "top"
                    (place-image (draw-cham "blue")
                                 (- 50 CHAM-HCENTER)
                                 (sub1 (- WORLD-HEIGHT CHAM-VCENTER))
                                 WORLD)))

(check-expect
 (render (make-cham 10 0.05 "green"))
 (place-image/align (draw-gauge 0.05)
                    0 GAUGE-FROM-TOP "left" "top"
                    (place-image (draw-cham "green")
                                 (- 10 CHAM-HCENTER)
                                 (sub1 (- WORLD-HEIGHT CHAM-VCENTER))
                                 WORLD)))

(define (render vc)
  (place-image/align
   (draw-gauge (cham-hap vc))
   0 GAUGE-FROM-TOP "left" "top"
   (place-image (draw-cham (cham-col vc))
                (- (cham-x vc) CHAM-HCENTER)
                (sub1 (- WORLD-HEIGHT CHAM-VCENTER))
                WORLD)))

;; Clock-tick-handler

; update-cham: VCham -> VCham
; Updates the VCham vc with every clock tick.

(check-expect
 (update-cham (make-cham 50 0.5 "blue"))
 (make-cham (modulo
             (+ 50 CHAM-SPEED)
             (+ WORLD-WIDTH CHAM-WIDTH))
            (- 0.5 UNHAPPINESS-SPEED)
            "blue"))

(check-expect
 (update-cham (make-cham 600 0.5 "blue"))
 (make-cham (modulo
             (+ 600 CHAM-SPEED)
             (+ WORLD-WIDTH CHAM-WIDTH))
            (- 0.5 UNHAPPINESS-SPEED)
            "blue"))

(define (update-cham vc)
  (make-cham (modulo
              (+ (cham-x vc) CHAM-SPEED)
              (+ WORLD-WIDTH CHAM-WIDTH))
             (ensure-hn (- (cham-hap vc) UNHAPPINESS-SPEED))
             (cham-col vc)))

;; Key event handler

; change-cham-color: VCham Color -> VCham
; Changes the color of a VCham vc to col.

(check-expect (change-cham-color (make-cham 50 0.5 "blue") "red")
              (make-cham 50 0.5 "red"))

(define (change-cham-color vc col)
  (make-cham (cham-x vc)
             (cham-hap vc)
             col))

; feed-cham: VCham -> VCham
; Feeds the VCham vc and increases its happiness by FEED-INDEX.

(check-expect (feed-cham (make-cham 50 0.5 "blue"))
              (make-cham 50 (+ 0.5 FEED-INDEX) "blue"))

(define (feed-cham vc)
  (make-cham (cham-x vc)
             (ensure-hn (+ (cham-hap vc) FEED-INDEX))
             (cham-col vc)))
             
; manage-cham: VCham KeyEvent -> VCham
; Feeds the chameleon or change its color.

(check-expect (manage-cham (make-cham 50 0.5 "blue") "r")
              (make-cham 50 0.5 "red"))

(check-expect (manage-cham (make-cham 50 0.5 "red") "b")
              (make-cham 50 0.5 "blue"))

(check-expect (manage-cham (make-cham 50 0.5 "red") "g")
              (make-cham 50 0.5 "green"))

(check-expect (manage-cham (make-cham 50 0.5 "red") "down")
              (make-cham 50 (+ 0.5 FEED-INDEX) "red"))

(define (manage-cham vc ke)
  (cond
    [(key=? ke "r") (change-cham-color vc "red")]
    [(key=? ke "b") (change-cham-color vc "blue")]
    [(key=? ke "g") (change-cham-color vc "green")]
    [(key=? ke "down") (feed-cham vc)]
    [else vc]))

;; End

; end?: VCham -> Boolean
; Ends the program if the VCham vc is too unhappy.

(check-expect (end? (make-cham 75 0 "red"))
              #true)

(check-expect (end? (make-cham 75 1 "red"))
              #false)

(define (end? vc)
  (<= (cham-hap vc) 0))

;; Main

; move-cham: VCham -> VCham
; Animates the chameleon.

(define (move-cham vc)
  (big-bang vc
            [to-draw render]
            [on-key manage-cham]
            [on-tick update-cham]
            [stop-when end?]))