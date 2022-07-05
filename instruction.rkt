#lang racket

(provide (struct-out inst))
(provide inst-size)

(struct inst (off0
              off1
              off2
              imm
              dst-reg
              op0-reg
              op1-addr
              res
              pc-update
              ap-update
              fp-update
              opcode) #:transparent)

(define (inst-size instruction)
  (if (equal? (inst-imm instruction) #f) 1 2))