#lang racket

(define input
  (for/vector ([line (in-lines)])
    (for/vector ([char (in-string line)])
      char)))

(define width (vector-length (vector-ref input 0)))

(define height (vector-length input))

(define (fixed-point f initial)
  (let ([next (f initial)])
    (if (equal? initial next) next (fixed-point f next))))

(define (in-range? seats pos)
  (and (< -1 (real-part pos) height) (< -1 (imag-part pos) width)))

(define (value-at seats pos)
  (vector-ref (vector-ref seats (real-part pos)) (imag-part pos)))

(define (occupied? seats max-depth start-coord)
  (λ (dir)
    (let iter ([pos (+ start-coord dir)] [depth max-depth])
      (cond [(or (zero? depth) (not (in-range? seats pos))) #f]
            [(eq? #\. (value-at seats pos)) (iter (+ pos dir) (sub1 depth))]
            [else (eq? #\# (value-at seats pos))]))))

(define dirs '(-1 -1+i +i 1+i 1 1-i -i -1-i))

(define (next max-depth threshold)
  (λ (seats)
    (for/vector ([(row-vector row) (in-indexed (in-vector seats))])
      (for/vector ([(value col) (in-indexed (in-vector row-vector))])
        (let* ([pos (make-rectangular row col)]
               [occupied (count identity (map (occupied? seats max-depth pos) dirs))])
          (cond [(and (zero? occupied) (eq? value #\L)) #\#]
                [(and (>= occupied threshold) (eq? value #\#)) #\L]
                [else value]))))))

(define (count-occupied seats)
  (for/sum ([row (in-vector seats)])
    (vector-count (curry eq? #\#) row)))

(define (part-1)
  (count-occupied (fixed-point (next 1 4) input)))

(define (part-2)
  (count-occupied (fixed-point (next +inf.0 5) input)))

(time (part-1))

(time (part-2))
