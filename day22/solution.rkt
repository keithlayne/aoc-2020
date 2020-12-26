#lang racket

(define input (string-split (string-trim (port->string)) "\n\n"))

(define deck1 (map string->number (cdr (string-split (car input) "\n"))))

(define deck2 (map string->number (cdr (string-split (cadr input) "\n"))))

(define (score deck)
  (apply + (map * deck (range (length deck) 0 -1))))

(define (part-1)
  (score
   (let iter ([d1 deck1] [d2 deck2])
     (cond [(null? d1) d2]
           [(null? d2) d1]
           [(> (car d1) (car d2)) (iter (append (cdr d1) (list (car d1) (car d2))) (cdr d2))]
           [else (iter (cdr d1) (append (cdr d2) (list (car d2) (car d1))))]))))

(define (part-2)
  (score
   (car
    (let iter ([d1 deck1] [d2 deck2] [seen (set)])
      (define next-seen (set-add seen (list d1 d2)))
      (define (p1-wins) (iter (append (cdr d1) (list (car d1) (car d2))) (cdr d2) next-seen))
      (define (p2-wins) (iter (cdr d1) (append (cdr d2) (list (car d2) (car d1))) next-seen))
      (cond
        [(null? d1) (cons d2 #f)]
        [(null? d2) (cons d1 #t)]
        [(set-member? seen (list d1 d2)) (cons d1 #t)]
        [(and (>= (length (cdr d1)) (car d1)) (>= (length (cdr d2)) (car d2)))
         (if (cdr (iter (take (cdr d1) (car d1)) (take (cdr d2) (car d2)) (set)))
             (p1-wins)
             (p2-wins))]
        [(> (car d1) (car d2)) (p1-wins)]
        [else (p2-wins)])))))

(time (part-1))

(time (part-2))
