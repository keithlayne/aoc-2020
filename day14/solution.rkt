#lang racket

(define input (port->lines))

(define (set-bit n m)
  (bitwise-ior (arithmetic-shift 1 m) n))

(define (unset-bit n m)
  (bitwise-and (bitwise-not (arithmetic-shift 1 m)) n))

(define (hash-set-keys hash keys value)
  (foldl (λ (key h) (hash-set h key value)) hash keys))

(define (solve get-addresses get-value)
  (for/fold ([mask ""] [memory (hash)] #:result (apply + (map cdr (hash->list memory))))
            ([line input])
    (match (string-split line " = ")
      [(list "mask" m) (values m memory)]
      [(list lhs val) (let ([address (string->number (car (regexp-match #px"\\d+" lhs)))]
                            [value (string->number val)])
                        (values mask
                                (hash-set-keys memory
                                               (get-addresses mask address)
                                               (get-value mask value))))])))

(define (part-1)
  (define (get-addresses _ address) (list address))
  (define (get-value mask value)
    (for/fold ([result value])
              ([(bit index) (in-indexed (in-string mask))])
      (cond [(char=? bit #\1) (set-bit result (- 35 index))]
            [(char=? bit #\0) (unset-bit result (- 35 index))]
            [else result])))
  (solve get-addresses get-value))

(define (part-2)
  (define (get-addresses mask address)
    (for/fold ([result (list address)])
              ([(bit index) (in-indexed (in-string mask))])
      (append-map (λ (value)
                    (define b (- 35 index))
                    (cond [(char=? bit #\1) (list (set-bit value b))]
                          [(char=? bit #\X) (list (set-bit value b) (unset-bit value b))]
                          [else (list value)]))
                  result)))
  (define (get-value _ value) value)
  (solve get-addresses get-value))

(time (part-1))

(time (part-2))
