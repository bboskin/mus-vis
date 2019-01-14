#lang racket

(require 2htdp/image)

(provide (all-defined-out))

;; constants
(define SCREEN-WIDTH 400)
(define SCREEN-HEIGHT 400)

(define (draw-scene c)
  (rectangle SCREEN-WIDTH SCREEN-HEIGHT "solid" c))

(define EMPTY-SCENE (draw-scene (make-color 0 0 0 0)))

(define gray-side-panel
  (rectangle (/ SCREEN-WIDTH 4) SCREEN-HEIGHT "solid" "gray"))

;; useful functions

(define (safe-sub1 x) (if (number? x) (max 0 (sub1 x)) 0))
(define (safe-add1 x) (if (number? x) (add1 x) 0))

(define (update-value s)
  (match s
    [`(,a ,f) (if f `(,(f a) ,f) `(,a #f))]))

(define (remove-ith i ls)
  (cond
    ((null? ls) '())
    ((zero? i) (cdr ls))
    (else (cons (car ls) (remove-ith (sub1 i) (cdr ls))))))

(define (snoc x ls)
  (foldr cons (list x) ls))
;; drawing text

(define (blank-text sel this if-yes if-no)
  (text (if (= sel this) if-yes if-no) 10 "black"))
(define (menu-text txt)
  (text
   (cond
     ((boolean? txt) (if txt "yes" "no"))
     ((number? txt) (number->string txt))
     ((symbol? txt) (symbol->string txt))
     ((color? txt)
      (match txt ((color r g b α)
                  (string-append "red:" (number->string r)
                                 "green:" (number->string g)
                                 "blue:" (number->string b)))))
     (else txt))
   10 "black"))

(define (draw-field mtext val i sel notext)
  (beside (menu-text mtext)
          (if val (menu-text val) (blank-text i sel "" notext))))

(define (draw-field/definitely mtext val)
  (beside (menu-text mtext)
          (menu-text val)))


;; color stuff

(define DEFAULT-PALETTE-LENGTH 80)
;; color constants
(define red (color 250 0 0))
(define orange (color 250 125 0))
(define yellow (color 250 250 0))
(define green (color 0 250 0))
(define blue (color 0 0 250))
(define purple (color 200 0 250))

(define black (color 0 0 0))
(define darkdarkgray (color 30 30 30))
(define darkgray (color 80 80 80))
(define gray (color 130 130 130))
(define lightgray (color 180 180 180))
(define lightlightgray (color 220 220 220))
(define white (color 255 255 255))

(define tan (color 200 150 100))
(define brown (color 130 100 80))

(define all-colors
  (list (list 'red red)
        (list 'orange orange)
        (list 'yellow yellow)
        (list 'green green)
        (list 'blue blue)
        (list 'purple purple)
        (list 'tan tan)
        (list 'brown brown)
        (list 'black black)
        (list 'darkdarkgray darkdarkgray)
        (list 'darkgray darkgray)
        (list 'gray gray)
        (list 'lightgray lightgray)
        (list 'lightlightgray lightlightgray)
        (list 'white white)))
;; palettes
(define rainbow (list red orange yellow green blue purple))
(define grayscale (list black darkdarkgray darkgray gray lightgray lightlightgray white))
(define sepia
  (let ((k (/ 50 DEFAULT-PALETTE-LENGTH)))
    (reverse (build-list DEFAULT-PALETTE-LENGTH
                         (λ (i) (color (floor (* i k 4)) (floor (* i k 3)) (floor (* i k 2))))))))

(define random-colors (build-list DEFAULT-PALETTE-LENGTH (λ (_) (color (random 250) (random 250) (random 250)))))
(define random/transparent (build-list DEFAULT-PALETTE-LENGTH (λ (_) (color (random 250) (random 250) (random 250) (random 250)))))

;; calculating gradients
(define (dist x y len) (/ (- y x) len))
(define (next-color a x Δ) (floor (+ a (* x Δ))))
(define (gradient2 len from to)
  (match* (from to)
    (((color r1 g1 b1 α1) (color r2 g2 b2 α2))
     (let ((Δr (dist r1 r2 len)) (Δg (dist g1 g2 len))
                                 (Δb (dist b1 b2 len)) (Δα (dist α1 α2 len)))
       (build-list len (λ (x) (color (next-color r1 x Δr) (next-color g1 x Δg)
                                     (next-color b1 x Δb) (next-color α1 x Δα))))))))


(define (gradient/ls colors)
  (let ((k (quotient DEFAULT-PALETTE-LENGTH (add1 (length colors)))))
    (let loop ((cs colors))
      (cond
        ((null? cs) '())
        ((null? (cdr cs)) cs)
        (else (append (gradient2 k (car cs) (cadr cs))
                      (loop (cdr cs))))))))

(define-syntax gradient
  (syntax-rules ()
    ((_ cl1 ...)
     (let* ((colors `(,cl1 ...)))
       (gradient/ls colors)))))

(define MAX-PAL-LEN 400)

(define (display-palette pal)
  (let ((k (/ MAX-PAL-LEN (max (length pal) 1))))
    (foldr (λ (x ans) (beside (rectangle k 100 "solid" x) ans)) empty-image pal)))