#lang racket

(require 2htdp/image "events.rkt")

(provide (all-defined-out))
#|
Macros and functions
to easily create common
visual pattern
|#

;; Stuff to do with grouping colors

(define-syntax make-color-scheme
  (syntax-rules ()
    ((_ name e ...) `(name (e ...)))))

(define primary-colors '(red blue yellow))
(define grayscale (build-list 50 (λ (x) (make-color x x x))))
(define rainbow '(red orange yellow green blue purple))
(define fluid (build-list 255 (λ (_) (make-color (random 255) (random 255) (random 255)))))
(define fluid-opacity (build-list 255 (λ (_) (make-color (random 255) (random 255) (random 255) (random 255)))))

(define (random-color options)
  (list-ref options (random (length options))))
