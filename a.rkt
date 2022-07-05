#lang racket

; (define A%
;   (class object%
;     (super-new)
;     (init qaq0)
;     (define qaq qaq0)
;     (define/public (out-qaq) (displayln qaq))
;     (field [b (new B%)])
;     )
;   )

; (define B%
;   (class object%
;     (super-new)
;     (field [c 0]))
;   )

; (struct C (f0 f1) #:transparent)

; (display (object? (C 0 1)))
; (define a (new A% [qaq0 (C 0 1)]))
; (send a out-qaq)

(define l (cons 3 (cons 2 (cons 1 null))))
(displayln (cdr l))
(displayln (list-ref l 0))
(displayln (list? l))

