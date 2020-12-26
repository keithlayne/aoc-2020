#lang racket

(define input (regexp-split #px"\n\n" (string-trim (port->string))))

(define groups
  (map (λ (line)
         (regexp-split #px"\n" line))
       input))

(define group-sets
  (for/list ([group groups])
    (map (λ (person)
           (apply set (string->list person)))
         group)))

(define (count-sets-by f)
  (for/sum ([sets group-sets])
    (set-count (apply f sets))))

(define (part-1)
  (count-sets-by set-union))

(define (part-2)
  (count-sets-by set-intersect))

(time (part-1))

(time (part-2))