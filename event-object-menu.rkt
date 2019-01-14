#lang racket

(require "events.rkt" "clusters.rkt" "helpers.rkt" "palettes.rkt" 2htdp/image)

(provide (all-defined-out))
(struct Shape (shape mode init-size final-size init-x init-y final-x final-y))

(struct EventObjMenuFields
  (name dur color-settings obj-kind obj selected-field))

(define blank-event-obj-menu
  (EventObjMenuFields "" 20 blank-color-settings #f #f 0))

(define (make-blank-kind kind)
  (match kind
    ['shape (Shape 'circle "solid" 0 0 100 100 100 100)]
    ['cluster blank-cluster]
    ['background 'background]
    [else #f]))

(define (next-kind kind)
  (match kind ['shape 'cluster] ['cluster 'background] [else 'shape]))
(define (prev-kind kind)
  (match kind ['background 'cluster] ['cluster 'shape] [else 'background]))


(define (num-fields obj-kind)
  (match obj-kind
    ['shape 13]
    ['cluster 18]
    ['background 5]
    [#f 4]))

(define (Shape->Component S dur pal C P)
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
            (make-component
             color-pkg
             `((shape ,sh #f)
               (size ,si-i ,Δ-size)
               (mode ,mode #f)
               (x ,x-i ,Δ-x)
               (y ,y-i ,Δ-y))))])))))



(define (Obj->Components k o dur pal C P)
  (match k
    ['shape (list (Shape->Component o dur pal C P))]
    ['cluster (Cluster->Components o dur pal C P)]
    [else (error "not ready yet")]))

(define (draw-obj-fields obj sel)
  (match obj
    ((Shape sh mode si-i si-f x-i y-i x-f y-f)
     (above/align "left"
      (draw-field "5. Shape:" sh 4 sel "Select shape")
      (draw-field "6. Initial size:" si-i 5 sel "Select initial size")
      (draw-field "7. Final size:" si-f 6 sel "Select final size")
      (draw-field "8. Initial x coord:" x-i 7 sel "Select initial x coord")
      (draw-field "9. Initial y coord:" y-i 8 sel "Select initial y coord")
      (draw-field "10. Final x coord:" x-f 9 sel "Select final x coord")
      (draw-field "11. Final y coord:" y-f 10 sel "Select final y coord")))
    ((Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f y-f t)
     (above/align "left"
      (draw-field "5. Shape:" sh 4 sel "Select shape")
      (draw-field "6. Color-mode:" c-m 5 sel "Select cluster size")
      (draw-field "7. Cluster-size:" n 6 sel "Select cluster size")
      (draw-field "8. Initial element size:" s-i 5 sel "Select initial size")
      (draw-field "9. Final element size:" s-f 6 sel "Select final size")
      (draw-field "8. Initial x coord:" x-i 7 sel "Select initial x coord")
      (draw-field "9. Initial y coord:" y-i 8 sel "Select initial y coord")
      (draw-field "10. Final x coord:" x-f 9 sel "Select final x coord")
      (draw-field "11. Final y coord:" y-f 10 sel "Select final y coord")
      (draw-field "12. Cluster tightness:" t 10 sel "Select cluster tightness")))
    (else empty-image)))

(define (flip-pal-mode p)
  (match p
    [(Color-settings mode pal-mode col pal)
     (Color-settings (match mode ['color 'palette] ['palette 'color]) pal-mode col pal)]))

(define (draw-color-mode c Cl Pa)
  (match c
    [(Color-settings mode pal-mode col pal)
     (match mode
       ['palette
        (above/align
         "left"
         (text (format "This element will use a ~s selection among colors in ~s" pal-mode (car (list-ref Pa pal))) 10 "black")
         (display-palette (cadr (list-ref Pa pal))))]
       ['color
        (above/align
         "left"
         (text (format "This element will use the color ~a" (car (list-ref Cl col))) 10 "black")
         (square 100 "solid" (cadr (list-ref Cl col))))])]))

(define (draw-event-obj-menu M Cl Pa Eo)
   (match M
     ((EventObjMenuFields name dur c obj-kind obj sel)
      (overlay
       (above/align "left"
        (text "Create a new Event Element!" 20 "black")
        (text "To save your current data, press return" 20 "black")
        (text (format "Currently you have selected field ~a" (add1 sel)) 20 "black")
        (draw-field "1. Element name:" name 0 sel "Enter name")
        (draw-field "2. Element duration:" dur 1 sel "Enter duration")
        (draw-color-mode c Cl Pa)
        ;; TODO add printing of selected palette
        (draw-field "4. Element kind:" obj-kind 3 sel "Select kind")
        (draw-obj-fields obj sel))
       (rectangle SCREEN-WIDTH SCREEN-HEIGHT "solid" "gray")))))

(define (scroll-pal-mode pal)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (Color-settings mode (match pal-mode ['random 'continuous] ['continuous 'random]) col pal)]))

(define (scroll-pal pal colors palettes)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (match mode
       ['color (Color-settings mode pal-mode (remainder (add1 col) (length colors)) pal)]
       ['palette (Color-settings mode pal-mode col (remainder (add1 pal) (length palettes)))])]))

(define (scroll-shape-shape dir sel sha)
  (match sha
    ((Shape sh mode si-i si-f x-i y-i x-f y-f)
     (match sel
       [4 (Shape (if dir (next-shape sh) (prev-shape sh)) mode si-i si-f x-i y-i x-f y-f)]
       [5 (Shape sh mode (if dir (safe-add1 si-i) (max 0 (safe-sub1 si-i))) si-f x-i y-i x-f y-f)]
       [6 (Shape sh mode si-i (if dir (safe-add1 si-f) (max 0 (safe-sub1 si-f))) x-i y-i x-f y-f)]
       [7 (Shape sh mode si-i si-f (if dir (safe-add1 x-i) (max 0 (safe-sub1 x-i))) y-i x-f y-f)]
       [8 (Shape sh mode si-i si-f x-i (if dir (safe-add1 y-i) (max 0 (safe-sub1 y-i))) x-f y-f)]
       [9 (Shape sh mode si-i si-f x-i y-i (if dir (safe-add1 x-f) (max 0 (safe-sub1 x-f))) y-f)]
       [10 (Shape sh mode si-i si-f x-i y-i x-f (if dir (safe-add1 y-f) (max 0 (safe-sub1 y-f))))]))))


(define (make-bckd pal dur P)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (match mode
       ['color (Background 0 (cadr (list-ref (Palette-menu-colors P) col)) dur #f)]
       [else (Background 0
                         (caadr (list-ref (Palette-menu-palettes P) pal))
                         dur
                         (cadr (list-ref (Palette-menu-palettes P) pal)))])]))

(define (scroll-shape dir sel obj-kind obj)
  (match obj-kind
    ['shape (scroll-shape-shape dir sel obj)]
    ['cluster (scroll-cluster dir sel obj)]
    [else obj]))

(define (scroll dir sel M)
  (match M
    ((EventObjMenuFields name dur pal obj-kind obj sel)
     (match sel
       [0 M]
       [1 (EventObjMenuFields name (if dir (safe-add1 dur) (safe-sub1 dur)) pal obj-kind obj sel)]
       [2 M]
       [3 (let* ((kind-name (if dir (next-kind obj-kind) (prev-kind obj-kind)))
                 (obj (make-blank-kind kind-name)))
            (EventObjMenuFields name dur pal kind-name obj sel))]
       [else (EventObjMenuFields name dur pal obj-kind (scroll-shape dir sel obj-kind obj) sel)]))))

(define (create-event-obj-key-handler i M Eo P)
  (match M
    ((EventObjMenuFields name dur pal obj-kind obj sel)
     (match i
       ["down" (let ((sel (remainder (add1 sel) (num-fields obj-kind))))
                 (values (EventObjMenuFields name dur pal obj-kind obj sel) Eo))]
       ["up" (let ((sel (if (zero? sel) (num-fields obj-kind) (sub1 sel))))
               (values (EventObjMenuFields name dur pal obj-kind obj sel) Eo))]
       ["."
        (match obj-kind
          ['background (values M `((,(string->symbol name) . ,(make-bckd pal dur P)) . ,Eo))]
          [else
           (let ((cs (Obj->Components obj-kind obj dur pal (Palette-menu-colors P) (Palette-menu-palettes P))))
             (values M `((,(string->symbol name) . ,(Event-Object dur cs)) . ,Eo)))])]
       ["left" (values (scroll #f sel M) Eo)]
       ["right" (values (scroll #t sel M) Eo)]
       ["=" (values (EventObjMenuFields name dur (flip-pal-mode pal) obj-kind obj sel) Eo)]
       ["`" (values (EventObjMenuFields name dur (scroll-pal-mode pal) obj-kind obj sel) Eo)]
       ["-" (values (EventObjMenuFields name dur (scroll-pal pal (Palette-menu-colors P) (Palette-menu-palettes P)) obj-kind obj sel) Eo)]
       [(? (λ (_) (= sel 0)))
        (match i
          [" " (values (EventObjMenuFields "" dur pal obj-kind obj sel) Eo)]
          [else (values (EventObjMenuFields (string-append name i) dur pal obj-kind obj sel) Eo)])]
       [else (values M Eo)]))))