#lang htdp/bsl

(require 2htdp/image)

;;; Exercise 1

(define x 3)
(define y 4)

(sqrt (+ (sqr x)
         (sqr y)))

;;; Exercise 2

(define prefix "hello")
(define suffix "world")

(string-append prefix "_" suffix)

;;; Exercise 3

(define str "helloworld")
(define i 5)

(string-append (substring str 0 i)
               "_"
               (substring str i))

;;; Exercise 4

(define str "helloworld")
(define i 5) ; i should be less than 10

(string-append (substring str 0 i)
               (substring str (add1 i)))

;;; Exercise 5

;; A car

(define unit 10)
(define wheel (circle (* 4 unit) "solid" "black"))

(define glass (rectangle (* 12 unit)
                         (* 5 unit)
                         "solid" "lightblue"))

(define body (rectangle (* 25 unit)
                        (* 7 unit)
                        "solid" "red"))

(overlay/xy
 glass (* -6 unit) (* 5 unit)
 (overlay/xy
  body 0 (* 3 unit)
  (overlay/xy wheel (* 17 unit) 0 wheel)))

;; A boat (draft)

(overlay/xy
 (overlay/xy
  (line 20 20 "black")
  20 20
  (line 30 0 "black"))
 50 0
 (line 20 -20 "black"))

;; A tree

(define unit 5)

(overlay/xy (circle (* 10 unit) "solid" "green")
            (* 7 unit) (* 18 unit)
            (rectangle (* 6 unit) (* 10 unit) "solid" "brown"))

;;; Exercise 6

(define cat (bitmap "files/cat.png"))

(* (image-width cat)
   (image-height cat))

;;; Exercise 7

(define sunny #true)
(define friday #false)

(or (not sunny)
    friday)

;;; Exercise 8

(define cat (bitmap "files/cat.png"))
(define cat-height (image-height cat))
(define cat-width (image-width cat))

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

;;; Exercise 9

(define in 10)

(if (string? in)
    (string-length in)
    (if (image? in)
        (* (image-width cat)
           (image-height cat))
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

;;; Exercise 10

;; Done! :)
