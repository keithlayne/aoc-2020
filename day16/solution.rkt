#lang racket

(define input (string-split (string-trim (port->string)) "\n\n"))

(define rules
  (for/hash ([line (in-list (string-split (car input) "\n"))])
    (match-let* ([(pregexp "^(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)" (list _ name xs ...)) line]
                 [(list a b c d) (map string->number xs)])
      (values name (λ (n) (or (<= a n b) (<= c n d)))))))

(define (parse-numbers line) 
  (map string->number (regexp-match* #px"\\d+" line)))

(define my-ticket (parse-numbers (cadr input)))

(define other-tickets
  (map parse-numbers (cdr (string-split (last input) "\n"))))

(define (matches? n)
  (for/or ([match? (in-hash-values rules)])
    (match? n)))

(define (part-1)
  (for*/sum ([ticket (in-list other-tickets)]
             [n (in-list ticket)]
             #:unless (matches? n))
    n))

(define (part-2)
  (define good
    (for/list ([ticket (in-list other-tickets)]
               #:when (andmap matches? ticket))
      ticket))
  (define possibles
    (for/list ([n (in-range (length my-ticket))])
      (for/set ([(name match?) (in-hash rules)]
                #:when (andmap (λ (ticket) (match? (list-ref ticket n))) good))
        name)))
  (define names
    (let iter ([lst possibles])
      (if (andmap string? lst)
          lst
          (iter (for/first ([names (in-list lst)]
                            #:when (and (set? names) (= 1 (set-count names))))
                  (for/list ([entry (in-list lst)])
                    (cond [(string? entry) entry]
                          [(equal? names entry) (set-first names)]
                          [else (set-remove entry (set-first names))])))))))
  (for/product ([n (in-list my-ticket)]
                [name (in-list names)]
                #:when (regexp-match? #px"^departure" name))
    n))

(time (part-1))

(time (part-2))
