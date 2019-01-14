#lang racket

#|
The user interface for this thingy
|#
(require "global-info.rkt"
         2htdp/image
         2htdp/universe)


(struct World
  (G
   menu-mode
   play?))
#|
(define (draw-menu)
  (rectangle (/ SCREEN-WIDTH 4) SCREEN-HEIGHT "solid" "gray"))

(define (draw-world w)
  (match w
    [(World G m play?) (beside (draw-menu m) (draw-Global G))]))



|#