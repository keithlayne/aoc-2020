#lang racket

(define-values (card-key door-key)
  (apply values (port->list)))

(define (loop subject value)
  (remainder (* subject value) 20201227))

(define (loop-size public-key)
  (let iter ([n 0] [value 1])
    (if (= value public-key)
        n
        (iter (add1 n) (loop 7 value)))))

(define (encryption-key public-key loops)
  (let iter ([n loops] [value 1])
    (if (zero? n)
        value
        (iter (sub1 n) (loop public-key value)))))

(define (part-1)
  (encryption-key door-key (loop-size card-key)))

(define (part-2) "There's no part 2")
                                      
(time (part-1))

(time (part-2))
