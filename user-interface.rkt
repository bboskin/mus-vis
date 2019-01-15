#lang racket

#|
The user interface for this thingy
|#
(require "events.rkt"
         "event-object-menu.rkt"
         "helpers.rkt"
         "event-menu.rkt"
         "palettes.rkt"
         2htdp/image
         2htdp/universe)


(struct World (ES Pal menu play? event-objs events saved-sequence))

(define-syntax λW
  (syntax-rules ()
    ((_ (ES Pal M P Eo E S) b)
     (λ (W)
       (match W
         [(World ES Pal M P Eo E S) b])))))


(define init-menus
  `(playing
    (playing . ())
    (create-event-obj . ,blank-event-obj-menu)
    (create-event . ,blank-event-menu)
    (create-palette . ,init-palette-menu)))

(define (get-menu-mode m) (car m))
(define (get-menu-live m) (cdadr m))
(define (get-menu-event-obj m) (cdaddr m))
(define (set-event-obj new m)
  (match m
    [`(,m ,live ,event-obj ,event ,pal)
     `(,m ,live (create-event-obj . ,new) ,event ,pal)]))

(define (get-menu-event m) (cdr (cadddr m)))
(define (set-event new m)
  (match m
    [`(,m ,live ,event-obj ,event ,pal)
     `(,m ,live ,event-obj (create-event . ,new) ,pal)]))


(define (next-menu s)
  (match s
    ('playing 'create-event-obj)
    ('create-event-obj 'create-event)
    ('create-event 'create-palette)
    ('create-palette 'playing)))

(define (prev-menu s)
  (match s
    ('create-event-obj 'playing)
    ('create-event 'create-event-obj)
    ('create-palette 'create-event)
    ('playing 'create-palette)))


(define init-World (World '() init-palette-menu init-menus #f '() '() '()))
(define World2 (World es1 init-palette-menu init-menus #f '() '() es1))

(define (draw-live-menu M) gray-side-panel)

(define (live-key-handler i)
  (λW (Q C m play? Eo E S)
      (match i
        ["p" (World Q C m (not play?) Eo E S)]
        ["r" (World S C m play? Eo E S)]
        [else (World Q C m play? Eo E S)])))

#|
Feature todos:
-- organize code/establish constants
-- make ordering of elements what we want.

-- graphs!

-- finish final screen/event saving

-- make it possible to have looping componenets change over time (the old feature deemed a bug)
-- get it synchronized with music, have ability to set intermediate restart points
|#

(define draw-world
  (λW (ES Pal M play? Eo E S)
      (match (get-menu-mode M)
        ('playing (beside (draw-live-menu (get-menu-live M)) (draw-event-sequence ES)))
        ('create-event-obj (draw-event-obj-menu (get-menu-event-obj M) (get-colors Pal) (get-palettes Pal) Eo))
        ('create-event (draw-event-menu (get-menu-event M) Eo))
        ('create-palette (draw-palette-menu Pal)))))


(define advance-live
  (λW (ES Pal m play? Eo E S)
      (if play? (World (advance-event-sequence ES) Pal m #t Eo E S)
          (World ES Pal m play? Eo E S))))

(define (advance-world W)
  (match (car (World-menu W))
    ['playing (advance-live W)]
    ['create-event (match W
                     [(World ES Pal m play? Eo E S)
                      (let ((m-new (advance-event-menu (get-menu-event m))))
                        (World ES Pal (set-event m-new m) play? Eo E S))])]
    [else W]))

(define (key-handler w i)
  ((λW (Q Pal m play? Eo E S)
       (match i
         ["[" (World Q Pal `(,(prev-menu (car m)) . ,(cdr m)) play? Eo E S)]
         ["]" (World Q Pal `(,(next-menu (car m)) . ,(cdr m)) play? Eo E S)]
         [else
          (match (car m)
            ['playing ((live-key-handler i) w)]
            ['create-event-obj (let-values (((m-new Eo) (create-event-obj-key-handler i (get-menu-event-obj m) Eo (get-colors Pal) (get-palettes Pal))))
                                 (World Q Pal (set-event-obj m-new m) play? Eo E S))]
            ['create-event (let-values (((m-new E) (create-event-key-handler i (get-menu-event m) Eo E)))
                             (World Q Pal (set-event m-new m) play? Eo E S))]
            ['create-palette (World Q (palette-key-handler Pal i) m play? Eo E S)])]))
   w))


(big-bang World2
  [on-tick advance-world]
  [to-draw draw-world]
  [on-key key-handler])