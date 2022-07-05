#lang racket

(require rackunit)
(require "run-context.rkt")
(require "decoder.rkt")
(require "instruction.rkt")
(require "relocatable.rkt")
(require "trace-entry.rkt")
(require "memory.rkt")
(require "memory-segment.rkt")

(provide VirtualMachine%)

(struct operands (dst res op0 op1) #:mutable #:transparent)

(define VirtualMachine%
  (class object%
    (super-new)
    (init prime0)
    (field [context (runcontext
                     (relocatable 0 0)
                     (relocatable 0 0)
                     (relocatable 0 0)
                     prime0)])
    (field [prime prime0])
    (init-field builtin-runners)



    (field [-program-base #f])
    (field [memory (new Memory%)])
    (field [validated-addresses #f])
    (define accessed-addresses (list))
    (field [trace (list)])
    (define current-step 0)
    (define skip-instruction-execution #f)

    ; return (values instruction imm)


    (define/public (get-instruction-encoding)
      (let* ([pc (runcontext-pc context)]
             [encoding (send memory get pc)]
             [imm-addr (add-usize-mod pc 1 #f)]
             [optional-imm (send memory get imm-addr)])
        ; (display "get-instruction-encoding: " )
        ; (displayln pc)
        (values encoding optional-imm)))

    (define (run-instruction instruction)
      (let-values ([(operands operands-mem-addresses) (compute-operands instruction)])
        (opcode-assertions instruction operands)
        (set! trace (cons (traceentry (runcontext-pc context) (runcontext-ap context) (runcontext-fp context)) trace ))
        (for ([addr operands-mem-addresses])
          (if (member addr accessed-addresses)
              #f
              (set! accessed-addresses (cons addr accessed-addresses))))
        (if (member (runcontext-pc context) accessed-addresses)
            #f
            (set! accessed-addresses (cons (runcontext-pc context) accessed-addresses)))
        (update-registers instruction operands)
        (set! current-step (add1 current-step))
        ))

    (define/public (update-fp instruction ops)
      (let ([new-fp (match (inst-fp-update instruction)
                      ['applus2 (add-usize-mod (runcontext-ap context) 2 #f)]
                      ['dst (operands-dst ops)]
                      ['regular (runcontext-fp context)])])
        (set-runcontext-fp! context new-fp)))

    (define/public (update-ap instruction ops)
      (let* ([ap (runcontext-ap context)]
             [new-ap (match (inst-ap-update instruction)
                       ['add (let ([res (operands-res ops)])
                               (check-not-equal? res #f "UnconstrainedResAdd")
                               (add-mod ap res prime))]
                       ['add1 (add-usize-mod ap 1 #f)]
                       ['add2 (add-usize-mod ap 2 #f)]
                       ['regular ap])])
        (set-runcontext-ap! context new-ap)))

    (define/public (update-pc instruction ops)
      (let* ([pc (runcontext-pc context)]
             [res (operands-res ops)]
             [new-pc (match (inst-pc-update instruction)
                       ['regular (add-usize-mod pc (inst-size instruction) prime)]
                       ['jump (begin
                                (check-not-equal? res #f "UnconstrainedResJump")
                                res)]
                       ['jumprel (begin
                                   (check-not-equal? res #f "UnconstrainedResJumpRel")
                                   (check-true (integer? res) "PureValue")
                                   (add-int-mod pc res prime))]
                       ['jnz (if (is-zero (operands-dst ops))
                                 (add-usize-mod pc (inst-size instruction) #f)
                                 (add-mod pc (operands-op1 ops) prime) )])])
        (set-runcontext-pc! context new-pc)))

    (define (update-registers instruction operands)
      (update-fp instruction operands)
      (update-ap instruction operands)
      (update-pc instruction operands))

    (define (is-zero addr)
      (check-true (integer? addr ) "PureValue")
      (equal? addr 0))

    (define (decode-current-instruction)
      (let-values ([(instruction imm) (get-instruction-encoding)])
        (decode-instruction instruction imm))
      )

    (define/public (step)
      ; (set! skip-instruction-execution #f)
      ; hint
      ; (displayln "")
      (let ([instruction (decode-current-instruction)]) (run-instruction instruction))
      #f)

    (define (deduce-op0 instruction dst op1)
      (match (inst-opcode instruction)
        ['call (values (add-usize-mod (runcontext-pc context) (inst-size instruction) #f) #f)]
        ['asserteq (if (or (equal? dst #f) (equal? op1 #f))
                       (values #f #f)
                       (match (inst-res instruction)
                         ['add (values (sub dst op1) dst)]
                         ['mul (if (and (integer? dst) (integer? op1))
                                   (if (not (equal? op1 0))
                                       (values (div-mod dst op1 prime) dst)
                                       (values #f #f))
                                   (values #f #f))]
                         [_ (values #f #f)]))]

        [_ (values #f #f)]))


    (define (deduce-op1 instruction dst op0)
      (match (inst-opcode instruction)
        ['asserteq (match (inst-res instruction)
                     ['op1 (if (equal? dst #f)
                               (values #f #f)
                               (values dst dst))]
                     ['add (if (or (equal? dst #f) (equal? op0 #f))
                               (values #f #f)
                               (values (sub dst op0) dst))]
                     ['mul (if (and (integer? dst) (integer? op0))
                               (if (not (equal? op0 0))
                                   (values (div-mod dst op0 prime) dst)
                                   (values #f #f))
                               (values #f #f))]
                     [_ (values #f #f)])]
        [_ (values #f #f)]))




    (define (compute-res instruction op0 op1)
      (match (inst-res instruction)
        ['op1 op1]
        ['add (add-mod op0 op1 prime)]
        ['mul (if (and (integer? op0) (integer? op1))
                  (mul-mod op0 op1 prime)
                  #f)]
        [_ #f]))

    (define (deduce-dst instruction res)
      (match (inst-opcode instruction)
        ['asserteq (if (equal? res #f) #f res)]
        ['call (runcontext-fp context)]
        [_ #f]))

    (define (opcode-assertions instruction ops)
      (match (inst-opcode instruction)
        ['asserteq (begin
                     (check-not-equal? (operands-res ops) #f "UnconstrainedResAssertEq")
                     (let* ([res (operands-res ops)]
                            [dst (operands-dst ops)])
                       (if (and (integer? res) (integer? dst))
                           (check-equal? res dst "DiffAssertValues")
                           #f)))]
        ['call (let* ([op0 (operands-op0 ops)]
                      [run-pc (runcontext-pc context)]
                      [return-pc (add-usize-mod run-pc (inst-size instruction) #f)]
                      [return-fp (runcontext-fp context)]
                      [dst (operands-dst ops)])
                 (check-equal? op0 return-pc "CantWriteReturnPc")
                 (check-equal? dst return-fp "CantWriteReturnFp"))]
        [_ #f]))

    (define (compute-operands instruction)
      ; (displayln instruction)
      (let* ([dst-addr (compute-dst-addr context instruction)]
             [dst (send memory get dst-addr)]
             [op0-addr (compute-op0-addr context instruction)]
             [op0 (send memory get op0-addr)]
             [op1-addr (compute-op1-addr context instruction op0)]
             [op1 (send memory get op1-addr)]
             [should-update-dst (equal? dst #f)]
             [should-update-op0 (equal? op0 #f)]
             [should-update-op1 (equal? op1 #f)] )
        (define res #f)
        (if (equal? op0 #f)
            (set!-values (op0 res) (deduce-op0 instruction dst op1))
            #f)
        (if (equal? op1 #f)
            (let-values ([(tmp-op1 tmp-res) (deduce-op1 instruction dst op0)])
              (set! op1 tmp-op1)
              (if (equal? res #f) (set! res tmp-res) #f))
            #f)
        (check-not-equal? op0 #f "Couldn't compute or deduce op0")
        (check-not-equal? op1 #f "Couldn't compute or deduce op1")
        (if (equal? res #f)
            (set! res (compute-res instruction op0 op1))
            #f)
        (if (equal? dst #f)
            (set! dst (match (inst-opcode instruction)
                        ['asserteq (begin (check-not-equal? res #f) res)]
                        ['call (runcontext-fp context)]))
            #f)


        ; (display "(dst, op0, op1, res): ")
        ; (displayln (list dst op0 op1 res))



        (if should-update-dst
            (send memory insert! dst-addr dst)
            #f)
        (if should-update-op0
            (send memory insert! op0-addr op0)
            #f)
        (if should-update-op1
            (send memory insert! op1-addr op1)
            #f)
        (values (operands dst res op0 op1) #(dst-addr op0-addr op1-addr))
        ))

    (define/public (create-segment _size)
      (send memory create-segment _size)
      )

    ; (define/public (compute-effective-sizes)
    ;   (send memory compute-effective-sizes)
    ;   )

    (define (relocate-trace relocation-table)
      (let ([relof ((curry relocate-value) relocation-table)])
        (define (reloftr tr)
          (traceentry (relof (traceentry-pc tr))
                      (relof (traceentry-ap tr))
                      (relof (traceentry-fp tr))))
        (map reloftr trace)
        ))

    (define/public (relocate)
      (let* ([sizes (send memory compute-effective-sizes)]
             [relo-table (send memory relocate-segments sizes)]
             [relo-mem (send memory relocate-memory relo-table)]
             )
        (values relo-mem (relocate-trace relo-table))
        ))
    ))

;test

(define (get-instruction-encoding-sucessful-without-imm)
  (let ([vm (new VirtualMachine% [prime0 37] [builtin-runners #f])])
    (let ([ctxt (get-field context vm)])
      (set-runcontext-pc! ctxt (relocatable 0 0)) )
    (let ([mem (get-field memory vm)])
      (send mem insert! (relocatable 0 0) 5))
    (let-values ([(encoding imm) (send vm get-instruction-encoding)])
      (check-equal? 5 encoding)
      (check-equal? #f imm))
    ))

(define (get-instruction-encoding-sucessful-with-imm)
  (let ([vm (new VirtualMachine% [prime0 37] [builtin-runners #f])])
    (let ([ctxt (get-field context vm)])
      (set-runcontext-pc! ctxt (relocatable 0 0)) )
    (let ([mem (get-field memory vm)])
      (send mem insert! (relocatable 0 0) 5)
      (send mem insert! (relocatable 0 1) 6))
    (let-values ([(encoding imm) (send vm get-instruction-encoding)])
      (check-equal? 5 encoding)
      (check-equal? 6 imm))
    ))

(define (update-fp-ap-plus2)
  (let* ([instruction (inst 1         ;off0
                            2         ;off1
                            3         ;off2
                            #f        ;imm
                            'fp       ;dst-reg
                            'ap       ;op0-reg
                            'ap       ;op1-addr
                            'add      ;res
                            'regular  ;pc-update
                            'regular  ;ap-update
                            'applus2  ;fp-update
                            'nop      ;opcode
                            )]
         [ops (operands 11 8 9 10)]
         [vm (new VirtualMachine% [prime0 37] [builtin-runners #f])])
    (let ([ctxt (get-field context vm)])
      (set-runcontext-pc! ctxt (relocatable 0 4))
      (set-runcontext-ap! ctxt (relocatable 0 5))
      (set-runcontext-fp! ctxt (relocatable 0 6))
      (send vm update-fp instruction ops)
      (check-equal? (runcontext-fp ctxt) (relocatable 0 7))))
  )


; (get-instruction-encoding-sucessful-without-imm)
; (get-instruction-encoding-sucessful-with-imm)
; (update-fp-ap-plus2)