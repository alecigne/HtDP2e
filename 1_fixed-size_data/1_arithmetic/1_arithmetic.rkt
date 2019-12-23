;; HtDP2e (v6.11.0.4 - https://www.htdp.org/2018-01-06/)
;; I.1 Fixed-Size Data / Arithmetic
;; Exercises 1-10

#lang htdp/bsl

(require 2htdp/image)

;; Exercise 1

(define x 3)
(define y 4)

(sqrt (+ (sqr x)
         (sqr y)))

;; Exercise 2

(define prefix "hello")
(define suffix "world")

(string-append prefix "_" suffix)

;; Exercise 3

(define str3 "helloworld")
(define i 5)

(string-append (substring str3 0 i)
               "_"
               (substring str3 i))

;; Exercise 4

(define str4 "helloworld")
(define j 5) ; i should be less than 10

(string-append (substring str4 0 j)
               (substring str4 (add1 j)))

;; Exercise 5

;; A car

(define car-unit 10)
(define wheel (circle (* 4 car-unit) "solid" "black"))

(define glass (rectangle (* 12 car-unit)
                         (* 5 car-unit)
                         "solid" "lightblue"))

(define body (rectangle (* 25 car-unit)
                        (* 7 car-unit)
                        "solid" "red"))

(overlay/xy
 glass (* -6 car-unit) (* 5 car-unit)
 (overlay/xy
  body 0 (* 3 car-unit)
  (overlay/xy wheel (* 17 car-unit) 0 wheel)))

;; A boat (draft)

;; (overlay/xy
;;  (overlay/xy
;;   (line 20 20 "black")
;;   20 20
;;   (line 30 0 "black"))
;;  50 0
;;  (line 20 -20 "black"))

;; A tree

(define tree-unit 5)

(overlay/xy (circle (* 10 tree-unit) "solid" "green")
            (* 7 tree-unit) (* 18 tree-unit)
            (rectangle (* 6 tree-unit) (* 10 tree-unit) "solid" "brown"))

;; Exercise 6

(define cat6 (bitmap "files/cat.png"))

(* (image-width cat6)
   (image-height cat6))

;; Exercise 7

(define sunny #true)
(define friday #false)

(or (not sunny)
    friday)

;; Exercise 8

(define cat8 (bitmap "files/cat.png"))
(define cat-height (image-height cat8))
(define cat-width (image-width cat8))

(if (> cat-width cat-height)
    "wide"
    "tall")

(if (> cat-width cat-height)
    "wide"
    (if (< cat-width cat-height)
        "tall"
        "square"))

(cond [(> cat-width cat-height) "wide"]
      [(< cat-width cat-height) "tall"]
      [else "square"])

;; Exercise 9

(define in 10)
(define cat9 (bitmap "files/cat.png"))

(if (string? in)
    (string-length in)
    (if (image? in)
        (* (image-width cat9)
           (image-height cat9))
        (if (number? in)
            (if (> in 0)
                (sub1 in)
                in)
            (if (boolean? in)
                (if in 10 20)
                "error"))))

(cond [(string? in) (string-length in)]
      [(image? in) (* (image-width in) (image-height in))]
      [(number? in) (if (> in 0) (sub1 in) in)]
      [(boolean? in) (if in 10 20)]
      [else "error"])

;; Exercise 10

;; Done! :)
