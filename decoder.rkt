#lang racket

(require rackunit)
(require "instruction.rkt")
(provide decode-instruction)

; 0|  opcode|ap_update|pc_update|res_logic|op1_src|op0_reg|dst_reg
; 15|14 13 12|    11 10|  9  8  7|     6  5|4  3  2|      1|      0

(define DST_REG_MASK #x0001)
(define DST_REG_OFF 0)
(define OP0_REG_MASK #x0002)
(define OP0_REG_OFF -1)
(define OP1_SRC_MASK #x001c)
(define OP1_SRC_OFF -2)
(define RES_LOGIC_MASK #x0060)
(define RES_LOGIC_OFF -5)
(define PC_UPDATE_MASK #x0380)
(define PC_UPDATE_OFF -7)
(define AP_UPDATE_MASK #x0c00)
(define AP_UPDATE_OFF -10)

(define OPCODE_MASK #x7000)
(define OPCODE_OFF -12)

; Flags start on the 48th bit.
(define FLAGS_OFFSET -48)
(define OFF0_OFF 0)
(define OFF1_OFF -16)
(define OFF2_OFF -32)
(define OFFX_MASK #xFFFF)

(define OFF_COMPL #x8000)

(define (decode-offset offset) (- offset OFF_COMPL))

(define (decode-instruction encoded-instr imm)
  (let* ([off0 (decode-offset (bitwise-and OFFX_MASK (arithmetic-shift encoded-instr OFF0_OFF)))]
         [off1 (decode-offset (bitwise-and OFFX_MASK (arithmetic-shift encoded-instr OFF1_OFF)))]
         [off2 (decode-offset (bitwise-and OFFX_MASK (arithmetic-shift encoded-instr OFF2_OFF)))]
         [flags (arithmetic-shift encoded-instr FLAGS_OFFSET)]
         [dst-reg-num (arithmetic-shift (bitwise-and flags DST_REG_MASK) DST_REG_OFF)]
         [op0-reg-num (arithmetic-shift (bitwise-and flags OP0_REG_MASK) OP0_REG_OFF)]
         [op1-src-num (arithmetic-shift (bitwise-and flags OP1_SRC_MASK) OP1_SRC_OFF)]
         [res-logic-num (arithmetic-shift (bitwise-and flags RES_LOGIC_MASK) RES_LOGIC_OFF)]
         [pc-update-num (arithmetic-shift (bitwise-and flags PC_UPDATE_MASK) PC_UPDATE_OFF)]
         [ap-update-num (arithmetic-shift (bitwise-and flags AP_UPDATE_MASK) AP_UPDATE_OFF)]
         [opcode-num (arithmetic-shift (bitwise-and flags OPCODE_MASK) OPCODE_OFF)]

         [dst-register (match dst-reg-num
                         [0 'ap]
                         [1 'fp])]
         [op0-register (match op0-reg-num
                         [0 'ap]
                         [1 'fp])]
         [op1-addr (match op1-src-num
                     [0 'op0]
                     [1 'imm]
                     [2 'fp]
                     [4 'ap])]
         [pc-update (match pc-update-num
                      [0 'regular]
                      [1 'jump]
                      [2 'jumprel]
                      [4 'jnz])]
         [res (match res-logic-num
                [0 (if (equal? pc-update 'jnz) 'unconstrained 'op1)]
                [1 'add]
                [2 'mul]
                )]
         [opcode (match opcode-num
                   [0 'nop]
                   [1 'call]
                   [2 'ret]
                   [4 'asserteq]
                   )]
         [ap-update (match ap-update-num
                      [0 (if (equal? opcode 'call) 'add2 'regular)]
                      [1 'add]
                      [2 'add1])]
         [fp-update (match opcode
                      ['call 'applus2]
                      ['ret 'dst]
                      [_ 'regular])]
         )

    (begin
      ;  (when (equal? opcode 'call)
      ;    (check-equal? ap-update 'add2))
      (let ([oimm (if (equal? op1-addr 'imm)
                      (begin (check-true (integer? imm)) imm)
                      #f)])
        (inst off0
              off1
              off2
              oimm
              dst-register
              op0-register
              op1-addr
              res
              pc-update
              ap-update
              fp-update
              opcode))
      )))

; (display (decode-instruction #x14A7000000000000 7))
; (display (decode-instruction #x0000800180007FFF #f))


; (decode-instruction #x14A7800080008000 7)

