#lang racket

(require "events.rkt"
         "helpers.rkt"
         2htdp/image)

(provide (all-defined-out))

(define blank-event-menu
  (list "" 50 #t blank-event-sequence #f blank-event-sequence))

(define event-menu-bckgd
  (draw-scene (color 20 20 20)))

(define (draw-event i e)
  (match e
    [(cons name x)
     (text (string-append (number->string i) ". " (symbol->string name)) 20 "black")]))

(define (event-menu-draw-events ls E)
  (let loop ((i 1)
             (E E))
    (cond
      ((null? E) empty-image)
      (else (above/align "left"
                         (draw-event i (car E))
                         (loop (add1 i) (cdr E)))))))


(define (advance-event-menu m)
  (match m
    [`(,name ,dur ,loop? ,seq ,playing? ,play-seq)
     (if playing?
         (if (>= (get-event-sequence-time play-seq) dur)
             (list name dur loop? seq loop? seq)
             (list name dur loop? seq #t (advance-event-sequence play-seq)))
         m)]))

(define (draw-event-menu m Eo)
  (match m
    (`(,name ,dur ,loop? ,seq ,playing? ,play-seq)
     (beside
       (overlay
        (above/align
         "left"
         (draw-field/definitely "Event name: " name)
         (draw-field/definitely "Event duration: " dur)
         (draw-field/definitely "Event repeat?: " loop?)
         (draw-field/definitely "Currently-playing?: " playing?)
         (event-menu-draw-events (build-list (length Eo) (λ (x) (add1 x))) Eo))
        gray-side-panel)
       (draw-event-sequence play-seq)))))

(define (create-event-key-handler i m Eo E)
  (match m
      (`(,name ,dur ,loop? ,seq ,playing? ,seq-playing)
       (match i
         [" " (values `(,name ,dur ,loop? ,seq ,(not playing?) ,seq-playing) E)]
         ["r" (values `(,name ,dur ,loop? ,seq #f ,seq) E)]
         ["l" (values `(,name ,dur ,(not loop?) ,seq ,playing? ,seq-playing) E)]
         ["0" (values `(,name ,dur ,loop? ,blank-event-sequence #f ,blank-event-sequence) E)]
         ["left" (values `(,name ,(max 0 (sub1 dur)) loop? ,seq ,playing? ,seq-playing) E)]
         ["right" (values `(,name ,(add1 dur) loop? ,seq ,playing? ,seq-playing) E)]
         [else (update-event-menu-sequences seq seq-playing i name dur loop? playing? E Eo)]         
         [else (values m E)]))))