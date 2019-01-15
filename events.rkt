#lang racket

(require "helpers.rkt"
         2htdp/image)

(provide
 blank-event-sequence
 draw-event-sequence
 advance-event-sequence
 get-event-sequence-time
 make-component
 make-background/color
 make-background/palette
 add-event-obj-to-list
 update-event-menu-sequences
 make-event-obj
 es1)

(struct Background (start-time color dur next))
(struct Component (color shape x y))
(struct Event-Sequence (curr-time active-events waiting-events bckgd))
(struct Event-Object (death cs))

(define (make-event-obj end params)
  (Event-Object end (make-component params)))

(define get-event-sequence-time Event-Sequence-curr-time)

(define black-bckgd (Background 0 (make-color 0 0 0) 0 #f))
(define blank-component (Component `(color ,red) `((#f #f) (#f #f) (#f #f)) '(#f #f) '(#f #f)))
(define (update-background b)
  (match b ((Background s c dur n)
            (if (and (> dur 0) n)
                (Background s (car n) (sub1 dur) (snoc (car n) (cdr n)))
                b))))

(define (draw-background b)
  (match b ((Background s c d n) (draw-scene c))))


(define blank-event-sequence
  (Event-Sequence 0 '() '() black-bckgd))

(define (make-background/color cls dur)
  (Background 0 cls dur #f))

(define (make-background/palette name dur p)
  (Background 0 name dur p))

(define (get-color-from-pkg p)
  (match p
    [`(color ,c) c]
    [`(palette ,i ,mode ,pal) (list-ref pal i)]))

(define (update-color-pkg p)
  (match p
    [`(color ,c) `(color ,c)]
    [`(palette ,i continuous ,p)
     `(palette ,(remainder (add1 i) (length p)) continuous ,p)]
    [`(palette ,i random ,p)
     `(palette ,(random (length p)) random ,p)]))


(define (make-component- args)
  (if (null? args) blank-component
    (let ((c (make-component- (cdr args))))
      (match (car args)
        [`(shape ,shape ,shape-f) (comp-set-shape-shape c shape shape-f)]
        [`(size ,s ,s-f) (comp-set-shape-size c s s-f)]
        [`(mode ,m ,m-f) (comp-set-shape-mode c m m-f)]
        [`(x ,x ,x-f) (comp-set-x c x x-f)]
        [`(y ,y ,y-f) (comp-set-y c y y-f)]))))

(define (make-component color-pck args)
  (let ((comp-no-color (make-component- args)))
    (match comp-no-color
      ((Component c s x y)
       (Component color-pck s x y)))))

(define (draw-shape c s)
  (match s
    (`((circle ,sh-f) (,size ,size-f) (,mode ,mode-f))
     (circle size mode c))
    (`((square ,sh-f) (,size ,size-f) (,mode ,mode-f))
     (square size mode c))
    (`((triangle ,sh-f) (,size ,size-f) (,mode ,mode-f))
     (triangle size mode c))))

(define (update-component c)
  (match c
    [(Component c s x y)
     (Component (update-color-pkg c) (map update-value s) (update-value x) (update-value y))]))


(define (comp-set-x C x x-f)
  (match C
    [(Component c s xi y) (Component c s `(,x ,x-f) y)]))

(define (comp-set-y C y y-f)
  (match C
    [(Component c s x yi) (Component c s x `(,y ,y-f))]))

(define (comp-set-shape-shape C sh sh-f)
  (match C
    [(Component c `(,ig . ,rst) x y)
     (Component c `((,sh ,sh-f) . ,rst) x y)]))

(define (comp-set-shape-size C si si-f)
  (match C
    [(Component c `(,a ,ig . ,rst) x y)
     (Component c `(,a (,si ,si-f) . ,rst) x y)]))

(define (comp-set-shape-mode C m m-f)
  (match C
    [(Component c `(,a ,b ,ig) x y)
     (Component c `(,a ,b (,m ,m-f)) x y)]))
#|
EventObjects are what are drawn,
created from events when events are ready to become active

EventObjects have
a) a time when they will be done and
b) a list of their components
|#

(define (add-event-obj-to-list name dur cs Eo)
  `((,(string->symbol name) . ,(Event-Object dur cs)) . ,Eo))

(define ((event-object-alive? cl) e)
  (match e
    [(Event-Object end cs) (or (false? end) (> end cl))]
    [else #t]))
(define (update-event-object e)
  (match e
    ((Event-Object d cs) (Event-Object d (map update-component cs)))
    ((Background s d n c) (update-background e))))

(define (draw-event-obj x e)
  (match e
    [(Event-Object end cs)
            (let loop ((cs cs))
              (match cs
                ('() EMPTY-SCENE)
                (`(,(Component c s x y) . ,rst)
                 (let ((curr-col (get-color-from-pkg c)))
                   (place-image (draw-shape curr-col s)
                                (car x) (car y)
                                (loop rst))))))]
    [else empty-image]))


(define eo1
  (Event-Object
   10
   (list (Component `(color ,yellow) `((square #f) (1 ,(λ (x) (add1 x))) (solid #f))
                    `(50 ,(λ (x) x))
                    `(0 ,(λ (x) (add1 x)))))))

(define eo2
  (Event-Object
   15
   (list (Component `(color ,green) `((square #f) (1 ,(λ (x) (+ 2 x))) (solid #f))
                    `(0 ,(λ (x) (add1 x)))
                    `(0 ,(λ (x) (+ 2 x)))))))

(define eo3
  (Event-Object
   #f
   (list (Component `(palette 0 random ,sepia)`((circle #f) (4 ,(λ (x) (random 20))) (solid #f))
                    `(0 ,(λ (x) (random SCREEN-WIDTH)))
                    `(0 ,(λ (x) (random SCREEN-HEIGHT))))
         (Component `(palette 0 random ,sepia) `((circle #f) (4 ,(λ (x) (random 20))) (solid #f))
                    `(0 ,(λ (x) (random SCREEN-WIDTH)))
                    `(0 ,(λ (x) (random SCREEN-HEIGHT))))
         (Component `(palette 0 random ,sepia) `((circle #f) (4 ,(λ (x) (random 20))) (solid #f))
                    `(0 ,(λ (x) (random SCREEN-WIDTH)))
                    `(0 ,(λ (x) (random SCREEN-HEIGHT)))))))


#|
Events are objects that have a start time, a duration, and a list of subpieces,
whose start times are in terms of whenever the event starts.
In addition, Events can loop, and their subpieces can be either Events or EventObjects
|#

(struct Event (start-time dur loop? events))

(define (instantiate-event-obj e cl)
  (match e
    [(Event-Object death draw-fn)
     (Event-Object (if (false? death) #f (+ death cl)) draw-fn)]))

(define (instantiate-events evs cl)
  (foldr (λ (e ans)
           (match e
             [(Event-Object death draw-fn)
              (cons (instantiate-event-obj e cl) ans)]
             [(Event start-time dur loop? es)
              (append (instantiate-events es cl) ans)]))
         '()
         evs))

(define (find-new-events cl es yes no bk)
  (match es
    ['() (values yes no bk)]
    [(cons (Background s c d n) rst)
     (if (= s cl)
         (find-new-events cl rst yes no (Background s c d n))
         (find-new-events cl rst yes (cons (Background s c d n) no) bk))]
    [(cons (Event s-t dur loop? events) rst)
     (let ((yes (if (= s-t cl) (append (instantiate-events events cl) yes) yes))
           (no (cond
                 ((> s-t cl) (cons (Event s-t dur loop? events) no))
                 ((and (<= s-t cl) loop?) (cons (Event (+ dur cl) dur loop? events) no))
                 (else no))))
       (find-new-events cl rst yes no bk))]))

(define ev1 (Event 0 20 #t (list eo1 eo2)))
(define ev2 (Event 50 1 #t (list eo3)))
(define blank-es (Event-Sequence 0 '() '() (Background 0 (make-color 0 0 0) 0 #f)))
(define es1 (Event-Sequence 0 '() (list ev1 ev2) (Background 0 (make-color 0 0 0) 0 #f)))

(define (draw-events x es bckgd)
  (foldr (λ (ea a)
           (overlay (draw-event-obj x ea) a))
         (draw-background bckgd)
         es))

(define (draw-event-sequence es)
  (match es
    [(Event-Sequence cl yes no bckgd)
     (draw-events cl yes bckgd)]))

(define (advance-event-sequence es)
  (match es
    [(Event-Sequence cl yes no bckgd)
     (let ((yes (map update-event-object (filter (event-object-alive? cl) yes))))
       (let-values (((to-add no bk) (find-new-events cl no '() '() (update-background bckgd))))
         (Event-Sequence (add1 cl) (append yes to-add) no bk)))]))




(define (update-event-menu-sequences seq seq-playing i name dur loop? playing? E Eo)
  (match* (seq seq-playing)
    [((Event-Sequence 0 '() ls bckgd1) (Event-Sequence cl2 yes2 no2 bckgd2))
     (match (string->number i)
       [(? number? i)
        (let ((curr (cdr (list-ref Eo (sub1 i)))))
          (match curr
            [(Background s d c n)
             (values (list name dur loop?
                           (Event-Sequence 0 '() (cons curr ls) bckgd1)
                           playing?
                           (Event-Sequence cl2 yes2 no2 curr))
                     E)]
            [else
             (let* ((eo (instantiate-event-obj (cdr (list-ref Eo (sub1 i))) 0))
                    (ev (Event cl2 dur loop? (list (instantiate-event-obj (cdr (list-ref Eo (sub1 i))) 0)))))
               (values (list name dur loop?
                             (Event-Sequence 0 '() (cons ev ls) bckgd1)
                             playing?
                             (Event-Sequence cl2 (cons eo yes2) no2 bckgd2))
                                  E))]))]
                    [else (values `(,(string-append name i) ,dur ,loop? ,seq ,playing? ,seq-playing) E)])]))