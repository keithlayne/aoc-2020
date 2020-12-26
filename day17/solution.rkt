#lang racket

(define input (port->lines))

(define x-limit (string-length (car input)))

(define y-limit (length input))

(define start-grid
  (for*/hash ([(line j) (in-indexed (in-list input))]
              [(c i) (in-indexed (in-string line))])
    (values (list i j 0 0) (char=? c #\#))))

(define (cycle 4-d? [cycles 6])
  (let iter ([n 1] [grid start-grid])
    (define (neighbor-count x y z w)
      (for*/sum ([l (in-range (max 0 (sub1 w)) (+ 2 w))]
                 [k (in-range (max 0 (sub1 z)) (+ 2 z))]
                 [j (in-range (sub1 y) (+ 2 y))]
                 [i (in-range (sub1 x) (+ 2 x))]
                 #:when (hash-ref grid (list i j k l) #f))
        (* (if (and (zero? w) (= l 1)) 2 1)
           (if (and (zero? z) (= k 1)) 2 1))))

    (define (active? x y z w)
      (let ([active (hash-ref grid (list x y z w) #f)]
            [neighbors (neighbor-count x y z w)])
        (or (and active (<= 3 neighbors 4))
            (and (not active) (= neighbors 3)))))

    (if (<= n cycles)
        (iter (add1 n)
              (for*/hash ([w (in-range (if 4-d? (add1 n) 1))]
                          [z (in-range (add1 n))]
                          [y (in-range (- n) (+ n y-limit))]
                          [x (in-range (- n) (+ n x-limit))]
                          #:when (active? x y z w))
                (values (list x y z w) #t)))
        (for*/sum ([w (in-range (if 4-d? (add1 cycles) 1))]
                   [z (in-range (add1 cycles))]
                   [y (in-range (- cycles) (+ cycles y-limit))]
                   [x (in-range (- cycles) (+ cycles x-limit))]
                   #:when (hash-ref grid (list x y z w) #f))
          (* (if (positive? z) 2 1) (if (positive? w) 2 1))))))

(define (part-1)
  (cycle #f))

(define (part-2)
  (cycle #t))

(time (part-1))

(time (part-2))
