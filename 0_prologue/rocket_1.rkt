#lang htdp/bsl

;;; Requires

(require 2htdp/image)
(require 2htdp/universe)

;;; Properties of the world and rocket

(define WIDTH 100)
(define HEIGHT 300)
(define ROCKET-SPEED 3)

;;; Graphical constants

(define ROCKET (bitmap "files/rocket.png"))
(define SCENE (empty-scene WIDTH HEIGHT))
(define ROCKET-XPOS (/ WIDTH 2))
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

;;; Functions

(define (place-rocket t)
  (if (<= (distance t) ROCKET-CENTER-TO-TOP)
      (place-image ROCKET
                   ROCKET-XPOS (distance t)
                   SCENE)
      (place-image ROCKET
                   ROCKET-XPOS ROCKET-CENTER-TO-TOP
                   SCENE)))

(define (distance t)
  (* ROCKET-SPEED t))

;;; Execution

(animate place-rocket)
