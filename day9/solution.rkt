#lang racket

(define input (list->vector (port->list)))

(define (valid? idx)
  (let ([slice (vector->list (vector-copy input (- idx 25) idx))])
    (for/or ([combo (in-combinations slice 2)])
      (= (vector-ref input idx) (apply + combo)))))

(define (part-1)
  (for/first ([n (in-range 25 (vector-length input))]
              #:unless (valid? n))
    (vector-ref input n)))

(define (part-2)
  (define bad (part-1))
  (define slice
    (for/or ([start (in-range 0 (sub1 (vector-length input)))])
      (let iter ([sum (vector-ref input start)]
                 [end (add1 start)])
        (cond [(= sum bad) (vector->list (vector-copy input start end))]
              [(= end (vector-length input)) #f]
              [else (iter (+ sum (vector-ref input end)) (add1 end))]))))
  (+ (apply max slice) (apply min slice)))

(time (part-1))

(time (part-2))
