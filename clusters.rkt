#lang racket

(require "helpers.rkt"
         "events.rkt"
         2htdp/image)

(provide
 blank-cluster
 Cluster->Components
 scroll-cluster
 draw-obj-fields-cluster)

(struct Cluster (shape group-shape color-mode count size-range init-size final-size init-x init-y final-x final-y tightness))
(define blank-cluster (Cluster 'circle 'circle 'random 20 15 10 50 500 500 500 500 7))

(define (make-cluster-next-loc-fns xi yi Δx Δy shape tightness)
  (let ((x-i (λ () (+ (random tightness) xi)))
        (y-i (λ () (+ (random tightness) yi)))
        (Δx (match shape
              ['col (λ (x) (Δx x))]
              [else (λ (x) (+ (random (add1 tightness)) (Δx x)))]))
        (Δy (match shape
              ['row (λ (y) (Δy y))]
              [else (λ (y) (+ (random (add1 tightness)) (Δy y)))])))
    (values x-i y-i Δx Δy)))

(define (make-cluster-next-size-fns s-r s-i Δs)
  (let ((s-i (λ () (+ s-i (random (add1 s-r))))))
    (values s-i (λ (x) (+ (random (add1 s-r)) (Δs x))))))

(define (Cluster->Components C dur pal Cs Ps)
  (match C
    [(Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f y-f t)
     (let ((Δx (calculate-change x-i x-f dur))
           (Δy (calculate-change y-i y-f dur))
           (Δs (calculate-change s-i s-f dur))
           (color-pkg (match pal
                        [(Color-settings c-mode pal-mode col pal)
                         (match c-mode
                             ['color `(color ,(cadr (list-ref Cs col)))]
                             ['palette `(palette 0 ,pal-mode ,(cadr (list-ref Ps pal)))])])))
       (let-values (((x-i y-i Δx Δy) (make-cluster-next-loc-fns x-i y-i Δx Δy g-s t))
                    ((s-i Δs) (make-cluster-next-size-fns s-r s-i Δs)))
         (build-list n
                   (λ (_) (make-component
                           color-pkg
                           `((shape ,sh #f)
                             (size ,(s-i) ,Δs)
                             (mode solid #f)
                             (x ,(x-i) ,Δx)
                             (y ,(y-i) ,Δy)))))))]))

(define (scroll-cluster dir sel clu)
  (match clu
    ((Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f y-f t)
     (match sel
       [4 (Cluster (if dir (next-shape sh) (prev-shape sh)) g-s c-m n s-r s-i s-f x-i y-i x-f y-f t)]
       [5 (Cluster sh (if dir (next-group-shape g-s) (prev-group-shape g-s)) c-m n s-r s-i s-f x-i y-i x-f y-f t)]
       [6 (Cluster sh g-s (if dir (next-color-mode c-m) (prev-color-mode c-m)) n s-r s-i s-f x-i y-i x-f y-f t)]
       [7 (Cluster sh g-s c-m (scroll-helper dir n) s-r s-i s-f x-i y-i x-f y-f t)]
       [8 (Cluster sh g-s c-m n (scroll-helper dir s-r) s-i s-f x-i y-i x-f y-f t)]
       [9 (Cluster sh g-s c-m n s-r (scroll-helper dir s-i) s-f x-i y-i x-f y-f t)]
       [10 (Cluster sh g-s c-m n s-r s-i (scroll-helper dir s-f) x-i y-i x-f y-f t)]
       [11 (Cluster sh g-s c-m n s-r s-i s-f (scroll-helper dir x-i) y-i x-f y-f t)]
       [12 (Cluster sh g-s c-m n s-r s-i s-f x-i (scroll-helper dir y-i) x-f y-f t)]
       [13 (Cluster sh g-s c-m n s-r s-i s-f x-i y-i (scroll-helper dir x-f) y-f t)]
       [14 (Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f (scroll-helper dir y-f) t)]
       [15 (Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f (scroll-helper dir t))]))))

(define (draw-obj-fields-cluster clu sel)
  (match clu
    [(Cluster sh g-s c-m n s-r s-i s-f x-i y-i x-f y-f t)
     (above/align "left"
      (draw-field "5. Shape:" sh 4 sel "Select shape")
      (draw-field "6. Color-mode:" c-m 5 sel "Select cluster size")
      (draw-field "7. Cluster-size:" n 6 sel "Select cluster size")
      (draw-field "8. Initial element size:" s-i 5 sel "Select initial size")
      (draw-field "9. Final element size:" s-f 6 sel "Select final size")
      (draw-field "10. Initial x coord:" x-i 7 sel "Select initial x coord")
      (draw-field "11. Initial y coord:" y-i 8 sel "Select initial y coord")
      (draw-field "12. Final x coord:" x-f 9 sel "Select final x coord")
      (draw-field "13. Final y coord:" y-f 10 sel "Select final y coord")
      (draw-field "14. Cluster tightness:" t 10 sel "Select cluster tightness"))]))