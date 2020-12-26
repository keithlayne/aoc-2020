#lang racket

(define input
  (map (match-lambda
         [(pregexp #px"^(\\d+)-(\\d+) (\\w): (\\w+)$" (list _ low high char password))
          (list (string->number low) (string->number high) (string-ref char 0) password)])
       (port->lines)))

(define (valid-part-1? low high char password)
  (define (match? c) (eq? char c))
  (<= low (sequence-count match? (in-string password)) high))

(define (valid-part-2? low high char password)
  (define (char-at idx) (string-ref password (sub1 idx)))
  (define (match? c) (eq? c char))
  (xor (match? (char-at low)) (match? (char-at high))))

(define (call-with f)
  (λ (args) (apply f args)))

(define (part-1)
  (count (call-with valid-part-1?) input))

(define (part-2)
  (count (call-with valid-part-2?) input))

(time (part-1))

(time (part-2))