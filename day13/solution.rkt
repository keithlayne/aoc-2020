#lang racket

(define input (port->lines))

(define pickup (string->number (car input)))

(define busses (map string->number (string-split (cadr input) ",")))

(define (part-1)
  (define (wait-time bus) (- bus (remainder pickup bus)))
  (define shortest (argmin wait-time (filter identity busses)))
  (* shortest (wait-time shortest)))

(define (part-2)
  (for/fold ([start-time 0] [factor 1] #:result start-time)
            ([(bus idx) (in-indexed (in-list busses))] #:when bus)
    (let iter ([m 0])
      (if (zero? (remainder (+ start-time idx (* m factor)) bus))
          (values (+ start-time (* m factor)) (* bus factor))
          (iter (add1 m))))))

(time (part-1))

(time (part-2))
