#lang racket

(provide (struct-out traceentry))
(struct traceentry (pc ap fp) #:transparent)