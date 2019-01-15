#lang racket
(require "helpers.rkt"
         "events.rkt"
         2htdp/image)

(provide blank-shape
         Shape->Components
         draw-obj-fields-shape
         scroll-shape)

(struct Shape
  (shape mode init-size final-size init-x init-y final-x final-y)
  #:mutable)

(define blank-shape (Shape INIT-SHAPE INIT-OUTLINE-MODE INIT-SHAPE-SIZE INIT-SHAPE-SIZE INIT-X-COORD INIT-Y-COORD INIT-X-COORD INIT-Y-COORD))

(define (Shape->Components S dur pal C P)
  (match S
    ((Shape sh mode si-i si-f x-i y-i x-f y-f)
     (let ((Δ-x (calculate-change x-i x-f dur))
           (Δ-y (calculate-change y-i y-f dur))
           (Δ-size (calculate-change si-i si-f dur)))
       (match pal
         [(Color-settings c-mode pal-mode col pal)
          (let ((color-pkg (match c-mode
                             ['color `(color ,(cadr (list-ref C col)))]
                             ['palette `(palette 0 ,pal-mode ,(cadr (list-ref P pal)))])))
            (list
             (make-component
             color-pkg
             `((shape ,sh #f)
               (size ,si-i ,Δ-size)
               (mode ,mode #f)
               (x ,x-i ,Δ-x)
               (y ,y-i ,Δ-y)))))])))))

(define (scroll-shape dir sel sha)
  (match sha
    ((Shape sh mode si-i si-f x-i y-i x-f y-f)
     (match sel
       [4 (set-Shape-shape! sha (if dir (next-shape sh) (prev-shape sh)))]
       [5 (set-Shape-init-size! sha (scroll-helper dir si-i))]
       [6 (set-Shape-final-size! sha (scroll-helper dir si-f))]
       [7 (set-Shape-init-x! sha (scroll-helper dir x-i))]
       [8 (set-Shape-init-y! sha (scroll-helper dir y-i))]
       [9 (set-Shape-final-x! sha (scroll-helper dir x-f))]
       [10 (set-Shape-final-y! sha (scroll-helper dir y-f))])
     sha)))

(define (draw-obj-fields-shape s sel)
  (match s
    [(Shape sh mode si-i si-f x-i y-i x-f y-f)
     (above/align "left"
      (draw-field "5. Shape:" sh 4 sel "Select shape")
      (draw-field "6. Initial size:" si-i 5 sel "Select initial size")
      (draw-field "7. Final size:" si-f 6 sel "Select final size")
      (draw-field "8. Initial x coord:" x-i 7 sel "Select initial x coord")
      (draw-field "9. Initial y coord:" y-i 8 sel "Select initial y coord")
      (draw-field "10. Final x coord:" x-f 9 sel "Select final x coord")
      (draw-field "11. Final y coord:" y-f 10 sel "Select final y coord"))]))