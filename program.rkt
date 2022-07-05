#lang racket

(require json)

(provide (struct-out program) deserialize-program)

(struct program (builtins prime data main hits) #:transparent)

(define (str->hex str)
  (string->number (string-append "#" (substring str 1)))
  )

(define (deserialize-program [path "tests/a.json"])
  (define in (open-input-file path))

  (let* ([js (read-json in)]
         [builtins (hash-ref js 'builtins)]
         [prime (hash-ref js 'prime)]
         [data (hash-ref js 'data)]
         [main (hash-ref (hash-ref (hash-ref js 'identifiers) '__main__.main) 'pc)]
         [hints (hash-ref js 'hints)])
    (program builtins
             (str->hex prime)
             (list->vector (map str->hex data))
             main
             hints)
    )
  )
