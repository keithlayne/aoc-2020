#lang racket

(define input (map string->number (regexp-match* #px"\\d" (port->string))))

(define (build-cups limit)
  (define input-length (length input))
  (define (next n)
    (define index (and (<= n input-length) (index-of input n)))
    (cond [(zero? n) 0]
          [(and index (< index (sub1 input-length))) (list-ref input (add1 index))]
          [index (if (> limit input-length) (+ 2 index) (car input))]
          [else (if (= n limit) (car input) (add1 n))]))
  (build-vector (add1 (apply max limit input)) next))

(define (game turns limit)
  (define highest (apply max limit input))
  (define cups (build-cups limit))
  (define (succ n) (vector-ref cups n))
  (define (removed-and-after current)
    (let iter ([n 4] [cup current] [result '()])
      (if (zero? n)
          (values (cdr result) (car result))
          (iter (sub1 n) (succ cup) (cons (succ cup) result)))))
  (define (destination current removed)
    (let iter ([cup (sub1 current)])
      (cond [(zero? cup) (iter highest)]
            [(member cup removed) (iter (sub1 cup))]
            [else cup])))
  (let iter ([current (car input)] [turn turns])
    (if (zero? turn)
        cups
        (let*-values ([(removed after) (removed-and-after current)]
                      [(dest) (destination current removed)])
          (vector-set! cups current after)
          (vector-set! cups (car removed) (succ dest))
          (vector-set! cups dest (last removed))
          (iter (succ current) (sub1 turn))))))

(define (part-1)
  (define (cups->list cups start)
    (let iter ([n (vector-ref cups start)] [result '()])
      (if (= start n)
          result
          (iter (vector-ref cups n) (cons n result)))))
  (for/sum ([digit (cups->list (game 100 0) 1)]
            [exp (in-naturals)])
    (* digit (expt 10 exp))))

(define (part-2)
  (define cups (game 10000000 1000000))
  (define next (vector-ref cups 1))
  (* next (vector-ref cups next)))

(time (part-1))

(time (part-2))
