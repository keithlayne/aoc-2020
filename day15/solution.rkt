#lang racket

(define input '(14 8 16 0 1 17))

(define (nth-number n)
  (define memo (make-hash (map cons input (range 1 (add1 (length input))))))
  (let iter ([number (last input)] [turn (length input)])
    (if (= turn n)
        number
        (let ([prev-turn (hash-ref memo number turn)])
          (hash-set! memo number turn)
          (iter (- turn prev-turn) (add1 turn))))))

(define (part-1)
  (nth-number 2020))

(define (part-2)
  (nth-number 30000000))

(time (part-1))

(time (part-2))
