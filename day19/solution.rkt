#lang racket

(define input (string-split (string-trim (port->string)) "\n\n"))

(define rules
  (for/hash ([rule (in-list (string-split (car input) "\n"))])
    (let* ([line (string-split rule ": ")]
           [rhs (cadr line)])
      (values (car line) (match rhs [(regexp "\"([a-z])\"" (list _ c)) c] [_ rhs])))))

(define messages (string-split (cadr input) "\n"))

(define (regexp-for rule)
  (let iter ([regex (hash-ref rules rule)])
    (if (not (regexp-match? "[0-9]" regex))
        (string-replace regex " " "")
        (iter (regexp-replace "[0-9]+" regex (λ (n) (string-append "(?:" (hash-ref rules n) ")")))))))

(define (count-matches str regex pos)
  (let iter ([matches 0] [p pos])
    (let ([m (regexp-match (string-append "^(?:" regex ")") (substring str p))])
      (if (not m)
          (values matches p)
          (iter (add1 matches) (+ (string-length (car m)) p))))))

(define (solve f)
  (define start-regexp (regexp-for "42"))
  (define end-regexp (regexp-for "31"))
  (define (match? s)
    (let*-values ([(a middle) (count-matches s start-regexp 0)]
                  [(b end) (count-matches s end-regexp middle)])
      (and (= (string-length s) end) (f a b))))
  (count match? messages))

(define (part-1) (solve (λ (a b) (< 0 b a 3))))

(define (part-2) (solve (λ (a b) (< 0 b a))))

(time (part-1))

(time (part-2))
