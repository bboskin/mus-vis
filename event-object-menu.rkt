#lang racket

(require "helpers.rkt"
         2htdp/image
         "clusters.rkt"
         "shapes.rkt"
         "events.rkt")

(provide
 blank-event-obj-menu
 draw-event-obj-menu
 create-event-obj-key-handler)

(struct EventObjMenuFields (name dur color-settings obj-kind obj selected-field))

(define blank-event-obj-menu (EventObjMenuFields "" INIT-DURATION blank-color-settings 'background 'background 0))

(define (make-blank-kind kind)
  (match kind
    ['shape blank-shape]
    ['cluster blank-cluster]
    ['background 'background]
    [else #f]))

(define (next-kind kind)
  (match kind
    ['shape 'cluster]
    ['cluster 'background]
    [else 'shape]))

(define (prev-kind kind)
  (match kind
    ['background 'cluster]
    ['cluster 'shape]
    [else 'background]))

(define (num-fields obj-kind)
  (match obj-kind
    ['shape 13]
    ['cluster 14]
    ['background 5]
    [#f 4]))

(define (Obj->Components k o dur pal C P)
  (match k
    ['shape (Shape->Components o dur pal C P)]
    ['cluster (Cluster->Components o dur pal C P)]
    [else (error "not ready yet")]))

(define (draw-obj-fields k obj sel)
  (match k
    ('shape (draw-obj-fields-shape obj sel))
    ('cluster (draw-obj-fields-cluster obj sel))
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
         (text (format "This element will use a ~s selection among colors in ~s" pal-mode (car (list-ref Pa pal))) TEXT-SIZE TEXT-COLOR)
         (display-palette (cadr (list-ref Pa pal))))]
       ['color
        (above/align
         "left"
         (text (format "This element will use the color ~a" (car (list-ref Cl col))) TEXT-SIZE TEXT-COLOR)
         (square 100 "solid" (cadr (list-ref Cl col))))])]))

(define (draw-event-obj-menu M Cl Pa Eo)
   (match M
     ((EventObjMenuFields name dur c obj-kind obj sel)
      (overlay
       (above/align "left"
        (text "Create a new Event Element!" TEXT-SIZE TEXT-COLOR)
        (text "To save your current data, press return" TEXT-SIZE TEXT-COLOR)
        (text (format "Currently you have selected field ~a" (add1 sel)) TEXT-SIZE TEXT-COLOR)
        (draw-field "1. Element name:" name 0 sel "Enter name")
        (draw-field "2. Element duration:" dur 1 sel "Enter duration")
        (draw-color-mode c Cl Pa)
        (draw-field "4. Element kind:" obj-kind 3 sel "Select kind")
        (draw-obj-fields obj-kind obj sel))
       (rectangle SCREEN-WIDTH SCREEN-HEIGHT "solid" "gray")))))


(define (flip-pal-color-mode pal-mode)
  (match pal-mode
    ['random 'continuous]
    ['continuous 'random]))

(define (scroll-pal-mode pal)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (Color-settings mode (flip-pal-color-mode pal-mode) col pal)]))

(define (scroll-pal pal colors palettes)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (match mode
       ['color (Color-settings mode pal-mode (remainder (add1 col) (length colors)) pal)]
       ['palette (Color-settings mode pal-mode col (remainder (add1 pal) (length palettes)))])]))


(define (make-bckd pal dur cols pals)
  (match pal
    [(Color-settings mode pal-mode col pal)
     (match mode
       ['color (make-background/color (cadr (list-ref cols col)) dur)]
       [else (let ((v (list-ref pals pal)))
               (make-background/palette (caadr v) dur (cadr v)))])]))

(define (scroll-kind dir sel obj-kind obj)
  (match obj-kind
    ['shape (scroll-shape dir sel obj)]
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
       [else (EventObjMenuFields name dur pal obj-kind (scroll-kind dir sel obj-kind obj) sel)]))))

(define (create-event-obj-key-handler i M Eo cols pals)
  (match M
    ((EventObjMenuFields name dur pal obj-kind obj sel)
     (match i
       ["down" (let ((sel (remainder (add1 sel) (num-fields obj-kind))))
                 (values (EventObjMenuFields name dur pal obj-kind obj sel) Eo))]
       ["up" (let ((sel (if (zero? sel) (num-fields obj-kind) (sub1 sel))))
               (values (EventObjMenuFields name dur pal obj-kind obj sel) Eo))]
       ["."
        (match obj-kind
          ['background (values M `((,(string->symbol name) . ,(make-bckd pal dur cols pals)) . ,Eo))]
          [else
           (let ((cs (Obj->Components obj-kind obj dur pal cols pals)))
             (values M (add-event-obj-to-list name dur cs Eo)))])]
       ["left" (values (scroll #f sel M) Eo)]
       ["right" (values (scroll #t sel M) Eo)]
       ["=" (values (EventObjMenuFields name dur (flip-pal-mode pal) obj-kind obj sel) Eo)]
       ["`" (values (EventObjMenuFields name dur (scroll-pal-mode pal) obj-kind obj sel) Eo)]
       ["-" (values (EventObjMenuFields name dur (scroll-pal pal cols pals) obj-kind obj sel) Eo)]
       [(? (λ (_) (= sel 0)))
        (match i
          [" " (values (EventObjMenuFields "" dur pal obj-kind obj sel) Eo)]
          [else (values (EventObjMenuFields (string-append name i) dur pal obj-kind obj sel) Eo)])]
       [else (values M Eo)]))))