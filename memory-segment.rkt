#lang racket

(require rackunit)
(require "relocatable.rkt")
(require "memory.rkt")

; (provide MemorySegmentManager%)
(provide load-data)

; (define MemorySegmentManager%
;   (class object%
;     (super-new)
;     (define num-segments 0)
;     (define segment-used-sizes null)

;     (define (create-segment memory _size)
;       (begin0
;         (relocatable num-segments 0)
;         (set! num-segments (add1 num-segments)))
;       )
;     ))

(define (load-data memory ptr data)
  (let ([len (vector-length data)])
    (for ([i (in-range len)])
      (send memory insert! (add-usize-mod ptr i #f) (vector-ref data i)))
    (add-usize-mod ptr len #f)
    ))