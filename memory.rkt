#lang racket

(require rackunit)
(require "relocatable.rkt")
(provide Memory%)

(define Memory%
  (class object%
    (super-new)
    (field [data (make-hash)])
    (define num-segments 0)
    ; (define segment-used-sizes null)

    (define/public (insert! key val)
      ; (displayln (list "insert! " key val))
      (check-false (hash-has-key? data key))
      (hash-set! data key val)
      )

    (define/public (get key)
      (if (hash-has-key? data key) (hash-ref data key) #f))

    (define/public (validate-existing-memory)
      ; TODO:validate
      #t)

    (define/public (create-segment _size)
      (begin0
        (relocatable num-segments 0)
        (set! num-segments (add1 num-segments)))
      )

    (define/public (compute-effective-sizes)
      (define size (make-vector num-segments 0))
      (define (proc key val)
        (check-true (relocatable? key))
        (let ([idx (relocatable-segment-index key)]
              [off (relocatable-offset key)])
          (vector-set! size idx (max (vector-ref size idx) (add1 off)))
          ))
      (hash-for-each data proc)
      size
      )

    (define/public (relocate-segments segment-used-sizes)
      (let ([relocation-table (make-vector (add1 num-segments) 0)])
        (for ([i (in-range num-segments)])
          (vector-set! relocation-table (add1 i)
                       (+ (vector-ref relocation-table i) (vector-ref segment-used-sizes i))))
        ; (displayln "")
        ; (displayln "relocation-table: ")
        ; (displayln relocation-table)
        relocation-table)
      )


    (define/public (relocate-memory relocation-table)
      (define relo-mem (make-vector
                        (add1 (vector-ref relocation-table (sub1 (vector-length relocation-table)))) 0))
      (define (proc key val)
        (check-true (relocatable? key))
        (let ([relof ((curry relocate-value) relocation-table)])
          (vector-set! relo-mem (relof key) (relof val))))
      (hash-for-each data proc)
      relo-mem
      )
    ))