#lang racket

(require rackunit)
(require "vm-core.rkt")
(require "relocatable.rkt")
(require "memory-segment.rkt")
(require "program.rkt")
(require "run-context.rkt")

(provide CairoRunner%)


(define CairoRunner%
  (class object%
    (super-new)
    (init prog0)
    (define prog prog0)
    (field [vm (new VirtualMachine%
                    [prime0 (program-prime prog)]
                    [builtin-runners #f])])
    (define _layout "plain")
    (define final-pc #f)
    (define prog-base #f)
    (define exec-base #f)
    (define initial-ap #f)
    (define initial-fp #f)
    (define initial-pc #f)
    ; (field [relocated-memory null])
    (field [relocated-trace #f])

    (define/public (initialize-segments program-base)
      (set! prog-base (if (equal? program-base #f)
                          (send vm create-segment #f)
                          program-base))
      (set! exec-base (send vm create-segment #f))
      ; TODO: builtin-runner
      )

    (define (initialize-state entrypoint stack)
      (check-true (relocatable? prog-base) "NoProgBase")
      (let ([pc (relocatable
                 (relocatable-segment-index prog-base)
                 (+ (relocatable-offset prog-base) entrypoint))])
        (set! initial-pc pc))
      (load-data (get-field memory vm) prog-base (program-data prog))
      (check-true (relocatable? exec-base) "NoExecBase")
      (load-data (get-field memory vm) exec-base stack)
      #f
      )

    (define (initialize-function-entrypoint entrypoint stack return-fp)
      (let ([end (send vm create-segment #f)])
        ; (set! stack (cons end (cons return-fp stack)))
        (set! stack (vector-append stack (vector return-fp end)))
        (check-true (relocatable? exec-base) "NoExecBaseForEntrypoint")
        (set! initial-fp (relocatable
                          (relocatable-segment-index exec-base)
                          (+ (relocatable-offset exec-base) (vector-length stack))))
        (set! initial-ap initial-fp)
        (initialize-state entrypoint stack)
        (set! final-pc end)
        end)
      )

    (define/public (initialize-main-entrypoint)
      (define stack (make-vector 0))
      ; TODO: builtin-runners
      (let* ([return-fp (send vm create-segment #f)]
             [main (program-main prog)])
        (check-true (integer? main) "MissingMain")
        (initialize-function-entrypoint main stack return-fp))
      )

    (define/public (initialize-vm)
      (check-true (relocatable? initial-pc) "NoPC")
      (check-true (relocatable? initial-ap) "NoAP")
      (check-true (relocatable? initial-fp) "NoFP")
      (let ([ctxt (get-field context vm)])
        (set-runcontext-pc! ctxt initial-pc)
        (set-runcontext-ap! ctxt initial-ap)
        (set-runcontext-fp! ctxt initial-fp))
      (check-true (relocatable? prog-base) "NoProgBase")
      (set-field! -program-base vm prog-base)
      ; TODO: builtin
      (let ([mem (get-field memory vm)])
        (check-true (send mem validate-existing-memory) "MemoryValidationError"))
      )

    (define/public (run-until-pc address)
      (if (equal? (runcontext-pc (get-field context vm)) address)
          #f
          (begin (send vm step)
                 (run-until-pc address))))

    (define/public (relocate)
      (send vm relocate))
    ))