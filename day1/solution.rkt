#lang racket

(define input (map string->number (port->lines)))

(define (solution lst [k 2])
  (for/first ([combo (in-combinations lst k)]
              #:when (= 2020 (apply + combo)))
    (apply * combo)))

(define (part-1)
  (solution input))

(define (part-2)
  (solution input 3))

(time (part-1))

(time (part-2))
