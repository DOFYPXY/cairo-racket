#lang racket

(require rackunit)
(require math/number-theory)

(provide relocate-value add-usize-mod add-int-mod add-mod sub div-mod mul-mod)

(struct relocatable (segment-index offset) #:transparent)
(provide (struct-out relocatable))

(define (relocate-value relocation-table value)
  (cond
    [(integer? value) value]
    [(relocatable? value)
     (let ([idx (relocatable-segment-index value)]
           [off (relocatable-offset value)])
       (check-true (> (vector-length relocation-table) idx))
       (add1 (+ (vector-ref relocation-table idx) off)))]
    [else #f]))


(define (add-int-mod self other prime)
  (if (integer? self)
      (remainder (+ self other) prime)
      (let ([big-offset (+ (relocatable-offset self) other)])
        (check-true (>= big-offset 0))
        (relocatable (relocatable-segment-index self) (remainder big-offset prime))))
  )

(define (add-usize-mod self other prime)
  (if (integer? self)
      (let ([num (+ self other)] )
        (if (integer? prime) (remainder num prime) (num)))
      (let ([new-offset (+ (relocatable-offset self) other)])
        (relocatable (relocatable-segment-index self) new-offset)))
  )

(define (add-mod self other prime)
  (match (cons (integer? self) (integer? other))
    [(cons #t #t) (remainder (+ self other) prime)]
    [(cons #f #f) (check-true #f)]
    [(cons #f #t) (let ([new-offset (+ (relocatable-offset self) other)])
                    (relocatable (relocatable-segment-index self) (remainder new-offset prime)))]
    [(cons #t #f) (let ([new-offset (+ (relocatable-offset other) self)])
                    (relocatable (relocatable-segment-index other) (remainder new-offset prime)))]
    ))

(define (sub self other)
  (match '((integer? self) (integer? other))
    ['(#t #t) (- self other)]
    ['(#f #f) (let ([seg (relocatable-segment-index self)])
                (check-equal? seg (relocatable-segment-index other))
                (relocatable seg (- (relocatable-offset self) (relocatable-offset other))))]
    [_ (check-true #f)]
    ))

(define (div-mod a b p)
  (with-modulus p (mod/ a b)))

(define (mul-mod a b p)
  (with-modulus p (mod* a b)))