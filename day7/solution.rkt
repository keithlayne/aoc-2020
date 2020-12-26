#lang racket

(define input (port->lines))

(define graph
  (for/hash ([line (in-list input)])
    (values (cadr (regexp-match #px"^(\\w+ \\w+) " line))
            (for/hash ([match (regexp-match* #px"(\\d+) (\\w+ \\w+) bag" line #:match-select cdr)])
              (values (cadr match) (string->number (car match)))))))

(define (part-1)
  (define (part-1* color)
    (for/fold ([colors (set)])
              ([(key value) (in-hash graph)]
               #:when (hash-has-key? value color))
      (set-add (set-union colors (part-1* key)) key)))
  (set-count (part-1* "shiny gold")))

(define (part-2)
  (define (part-2* color)
    (for/sum ([(key value) (in-hash (hash-ref graph color))])
      (* value (add1 (part-2* key)))))
  (part-2* "shiny gold"))

(time (part-1))

(time (part-2))
