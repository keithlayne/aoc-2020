#lang racket

(define input (cons 0 (sort (port->list) <)))

(define (part-1)
  (for/fold ([ones 0] [threes 1] #:result (* ones threes))
            ([a (cdr input)] [b input])
    (cond [(= 3 (- a b)) (values ones (add1 threes))]
          [(= 1 (- a b)) (values (add1 ones) threes)])))

(define (memoize f)
  (define memo (make-hash))
  (λ args
    (unless (hash-has-key? memo args) (hash-set! memo args (apply f args)))
    (hash-ref memo args)))

(define part-2
  (memoize
   (λ (n)
     (define (reachable? x) (< n x (+ n 4)))
     (max 1 (apply + (map part-2 (filter reachable? input)))))))

(time (part-1))

(time (part-2 0))
