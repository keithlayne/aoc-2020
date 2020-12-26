#lang racket

(define input
  (vector-sort
   (for/vector ([line (in-lines)])
     (let* ([str (regexp-replace* #rx"[FL]" line "0")]
            [str (regexp-replace* #rx"[BR]" str "1")])
       (string->number str 2))) 
   <))
    
(define (part-1)
  (vector-ref input (sub1 (vector-length input))))

(define (part-2)
  (for/first ([seat (in-vector input)]
              [index (in-naturals (vector-ref input 0))]
              #:unless (= seat index))
    index))

(time (part-1))

(time (part-2))
