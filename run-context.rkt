#lang racket

(require rackunit)
(require "instruction.rkt")
(require "relocatable.rkt")

(provide (struct-out runcontext))
(provide compute-dst-addr compute-op0-addr compute-op1-addr)

(struct runcontext (pc ap fp prime) #:mutable #:transparent)

(define (compute-dst-addr self instruction)
  (let ([base-addr (match (inst-dst-reg instruction)
                     ['ap (runcontext-ap self)]
                     ['fp (runcontext-fp self)])])
    (add-int-mod base-addr (inst-off0 instruction) (runcontext-prime self)))
  )

(define (compute-op0-addr self instruction)
  (let ([base-addr (match (inst-op0-reg instruction)
                     ['ap (runcontext-ap self)]
                     ['fp (runcontext-fp self)])])
    (add-int-mod base-addr (inst-off1 instruction) (runcontext-prime self)))
  )

(define (compute-op1-addr self instruction op0)
  (let ([base-addr (match (inst-op1-addr instruction)
                     ['fp (runcontext-fp self)]
                     ['ap (runcontext-ap self)]
                     ['imm (begin
                             (check-eq? (inst-off2 instruction) 1 "Imm should be 1.")
                             (runcontext-pc self))]
                     ['op0 (begin
                             (check-true (or (integer? op0) (relocatable? op0)))
                             op0)]
                     )])

    (add-int-mod base-addr (inst-off2 instruction) (runcontext-prime self)))
  )


