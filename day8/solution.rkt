#lang racket

(define input (string-trim (port->string)))

(define program
  (for/vector ([line (regexp-split #px"\n" input)])
    (match (string-split line) [(list op arg) (cons op (string->number arg))])))

(define (run prog)
  (let iter ([acc 0] [pc 0] [seen (set)])
    (cond [(= pc (vector-length prog)) (cons #t acc)]
          [(set-member? seen pc) (cons #f acc)]
          [else (match (vector-ref prog pc)
                  [(cons "acc" arg) (iter (+ acc arg) (add1 pc) (set-add seen pc))]
                  [(cons "jmp" arg) (iter acc (+ pc arg) (set-add seen pc))]
                  [_ (iter acc (add1 pc) (set-add seen pc))])])))

(define (part-1)
  (cdr (run program)))
  
(define (part-2)
  (define (fix idx)
    (let ([copy (vector-copy program)]
          [op (vector-ref program idx)])
      (vector-set! copy idx (cons (if (eq? (car op) "nop") "jmp" "nop") (cdr op)))
      copy))
  (for/first ([(op idx) (in-indexed (in-vector program))]
              #:when (regexp-match? "nop|jmp" (car op))
              [result (in-value (run (fix idx)))]
              #:when (car result))
    (cdr result)))

(time (part-1))

(time (part-2))
