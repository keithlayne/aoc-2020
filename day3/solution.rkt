#lang racket

(define input
  (for/vector ([line (in-lines)])
    (for/vector ([c (in-string line)])
      (eq? #\# c))))

(define width (vector-length (vector-ref input 0)))

(define height (vector-length input))

(define (tree-at? slope)
  (λ (n)
    (vector-ref (vector-ref input (* n (cadr slope)))
                (remainder (* n (car slope)) width))))

(define (solution slopes)
  (for/product ([slope slopes])
    (count (tree-at? slope) (range 0 (/ height (cadr slope))))))

(define (part-1)
  (solution '((3 1))))

(define (part-2)
  (solution `((1 1) (3 1) (5 1) (7 1) (1 2))))

(time (part-1))

(time (part-2))
