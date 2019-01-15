#lang racket

(require "helpers.rkt" 2htdp/image)
(provide
 init-palette-menu
 draw-palette-menu
 palette-key-handler
 get-colors
 get-palettes)


(struct Palette-menu
  (colors palettes gradient? mode col-name editing-col cl-sel pal-name editing-pal))

(define get-colors Palette-menu-colors)
(define get-palettes Palette-menu-palettes)

(define init-palette-menu
  (Palette-menu
   all-colors
   `((rainbow ,rainbow) (grayscale ,grayscale) (sepia ,sepia) (random-colors ,random-colors) (random-transparent ,random/transparent))
   #f 'palette
   "" (color 200 100 0 255) 'red "" '()))

(define (flip-palette-mode m)
  (match m
    ['palette 'color]
    ['color 'palette]))


(define (drop-palette-item mode cls pal)
  (match mode
    ['palette (values cls (if (null? pal) '() (cdr pal)))]
    ['color (values (if (null? cls) '() (cdr cls)) pal)]))

(define (add-palette-item mode pal)
  (match mode
    ['palette (cons 0 pal)]
    ['color pal]))

(define (prev-color-selection cs)
  (match cs
    ['α 'red]
    ['red 'blue]
    ['blue 'green]
    ['green 'α]))

(define (next-color-selection cs)
  (match cs
    ['blue 'α]
    ['green 'blue]
    ['red 'green]
    ['α 'red]))

(define (increase-color-val cl col-sel)
  (match cl
    [(color r g b α)
     (match col-sel
       ['red (color (remainder (add1 r) 255) g b α)]
       ['green (color r (remainder (add1 g) 255) b α)]
       ['blue (color r g (remainder (add1 b) 255) α)]
       ['α (color r g b (remainder (add1 α) 255))])]))

(define (decrease-color-val cl col-sel)
  (match cl
    [(color r g b α)
     (match col-sel
       ['red (color (if (zero? r) 255 (sub1 r)) g b α)]
       ['green (color r (if (zero? g) 255 (sub1 g)) b α)]
       ['blue (color r g (if (zero? b) 255 (sub1 b)) α)]
       ['α (color r g b (if (zero? α) 255 (sub1 α)))])]))

(define (palette-key-handler pal i)
  (match pal
    [(Palette-menu cls pals gradient? mode cl-name ed-cls col-sel pal-name ed-pal)
     
     (match* (i mode)
       [("`" _) (Palette-menu cls pals (not gradient?) mode cl-name ed-cls col-sel pal-name ed-pal)]
       [("/" _) (Palette-menu cls pals gradient? (flip-palette-mode mode) cl-name ed-cls col-sel pal-name ed-pal)]
       [("left" 'palette)
        (let ((ed-pal (if (null? ed-pal) '() (cdr ed-pal))))
          (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel pal-name ed-pal))]
       [("right" 'palette)
        (let ((ed-pal (cons 0 ed-pal)))
          (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel pal-name ed-pal))]
       [("left" 'color)
        (let ((col-sel (prev-color-selection col-sel)))
          (Palette-menu cls pals gradient? 'color cl-name ed-cls col-sel pal-name ed-pal))]
       [("right" 'color)
        (let ((col-sel (next-color-selection col-sel)))
          (Palette-menu cls pals gradient? 'color cl-name ed-cls col-sel pal-name ed-pal))]
       [("up" 'palette)
        (let ((ed-pal (if (null? ed-pal) '() (cons (remainder (add1 (car ed-pal)) (length cls)) (cdr ed-pal)))))
          (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel pal-name ed-pal))]
       [("down" 'palette)
        (let ((ed-pal (if (null? ed-pal) '() (cons (if (zero? (car ed-pal)) (sub1 (length cls)) (sub1 (car ed-pal))) (cdr ed-pal)))))
          (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel pal-name ed-pal))]
       [("up" 'color)
        (let ((ed-cls (increase-color-val ed-cls col-sel)))
          (Palette-menu cls pals gradient? 'color cl-name ed-cls col-sel pal-name ed-pal))]
       [("down" 'color)
        (let ((ed-cls (decrease-color-val ed-cls col-sel)))
          (Palette-menu cls pals gradient? 'color cl-name ed-cls col-sel pal-name ed-pal))]
       [("." 'palette)
        (let ((pally (map (λ (x) (cadr (list-ref cls x))) ed-pal)))
          (Palette-menu cls (cons (list (string->symbol pal-name) (if gradient? (gradient/ls pally) pally)) pals) #t 'palette cl-name ed-cls col-sel "" '()))]
       [("." 'color)
        (Palette-menu (cons (list (string->symbol cl-name) ed-cls) cls) pals #t 'color "" (color 200 200 200) 'red pal-name ed-pal)]
       [(" " 'palette) (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel "" ed-pal)]
       [(" " 'color) (Palette-menu cls pals gradient? 'color "" ed-cls col-sel pal-name ed-pal)]
       [(_ 'palette) (Palette-menu cls pals gradient? 'palette cl-name ed-cls col-sel (string-append pal-name i) ed-pal)]
       [(_ 'color) (Palette-menu cls pals gradient? 'color (string-append cl-name i) ed-cls col-sel pal-name ed-pal)]
       [(_ _) pal])]))

(define (draw-palette-menu-name-message mode cl pal)
  (text (string-append (format "The ~s name is: " (symbol->string mode))
                       (match mode
                         ['palette pal]
                         ['color cl]))
        20 "black"))

(define (palette-draw-color-mode col-sel cl name)
  (match cl
     [(color r g b α)
      (above/align
       "left"
       (text (format "the name of this color is ~s" name) 20 "black")
       (text (format "Selected the ~s field." col-sel) 20 "black")
       (text (format "your color's red field is ~s" r) 20 "black")
       (text (format "your color's green field is ~s" g) 20 'black)
       (text (format "your color's blue field is ~s" b) 20 'black)
       (text (format "your color's transparency field is ~s" α) 20 'black)
       (square 200 "solid" cl))]))

(define (palette-draw-palette-mode g? pal name colors)
  (above/align
   "left"
   (text (if g? "Palette will be a gradient" "Palette will be an enumeration") 20 "black")
   (text (format "the name of this palette is ~s" name) 20 "black")
   (text "the colors in the palette are:" 20 black)
   (text (foldr (λ (x ans) (string-append (symbol->string (car (list-ref colors x))) ", " ans)) "" pal) 20 "black")
   (let ((cc (map (λ (x) (cadr (list-ref colors x))) pal)))
     (display-palette (if g? (gradient/ls cc) cc)))))

(define (draw-palette-menu pal)
  (match pal
    [(Palette-menu cls pals gradient? mode cl-name ed-cls col-sel pal-name ed-pal)
     
     (above/align
      "left"
      (text (format "Editing in ~s mode" (symbol->string mode)) 20 "black")
      (text "To switch modes, press /" 20 "black")
      (if (eqv? mode 'color)
          (palette-draw-color-mode col-sel ed-cls cl-name)
          (palette-draw-palette-mode gradient? ed-pal pal-name cls)))]))