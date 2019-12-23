;; HtDP2e (v6.11.0.4 - https://www.htdp.org/2018-01-06/)
;; I.2.1 Fixed-Size Data / Functions and Programs / Functions
;; Exercises 11-20

#lang htdp/bsl

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;; Exercise 11

(define (distance-to-origin x y)
  (sqrt (+ (sqr x)
           (sqr y))))

;; Exercise 12

(define (cvolume a)
  (expt a 3))

(define (csurface a)
  (* 6 (sqr a)))

;; Exercise 13

(define (string-first str)
  (string-ith str 0))

;; Exercise 14

(define (string-last str)
  (string-ith str (sub1 (string-length str))))

;; Exercise 15

(define (==> sunny friday)
  (or (not sunny)
      friday))

;; Exercise 16

(define (image-area img)
  (* (image-width img)
     (image-height img)))

;; Exercise 17

(define (image-classify img)
  (cond [(> (image-height img) (image-width img)) "tall"]
        [(< (image-height img) (image-width img)) "wide"]
        [#true "square"]))

;; Exercise 18

(define (string-join str1 str2)
  (string-append str1 "_" str2))

;; Exercise 19

(define (string-insert str i)
  (string-append (substring str 0 i)
                 "_"
                 (substring str i)))

;; Exercise 20

(define (string-delete str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))
