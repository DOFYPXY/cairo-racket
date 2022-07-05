#lang racket

(require "cairo-runner.rkt")
(require "program.rkt")
(require "write.rkt")


(define (cairo-run path)
  (define prog (deserialize-program path))
  (define runner (new CairoRunner% [prog0 prog]))
  (send runner initialize-segments #f)
  (define end (send runner initialize-main-entrypoint))
  (send runner initialize-vm)
  ;   (display "end: ")
  ;   (displayln end)
  (send runner run-until-pc end)
  ; TODO: verify
  (let-values ([(relo-mem relo-tr) (send runner relocate)])
    ; (displayln relo-mem)
    ; (display relo-tr)
    (write-mem relo-mem)
    (write-trace relo-tr)
    )
  )

(define (main)
  (let ([path (vector-ref (current-command-line-arguments) 0)])
    (cairo-run path)))


; (cairo-run "tests/foo3.json")
(main)