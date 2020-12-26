#lang racket

(define input (port->lines))

; TODO: Suck less

(define stuff (make-hash))

(for ([line (in-list input)])
  (match-let* ([(pregexp "(.*) \\(contains (.*)\\)" (list _ lhs rhs)) line])
    (let* ([ingredients (string-split lhs " ")]
           [allergens (string-split rhs ", ")])
      (for ([allergen (in-list allergens)])
        (let ([old (hash-ref stuff allergen #f)])
          (if old
              (hash-set! stuff allergen (set-intersect old (apply set ingredients)))
              (hash-set! stuff allergen (apply set ingredients))))))))

(define all (apply set-union (hash-values stuff)))

(define (part-1)
  (for/sum ([line (in-list input)])
    (match-let* ([(pregexp "(.*) \\(contains (.*)\\)" (list _ lhs _)) line])
      (count (λ (i) (not (set-member? all i))) (string-split lhs " ")))))
    
(define (part-2)
  (let iter ()
    (cond [(andmap string? (hash-values stuff)) (string-join (map (λ (a) (hash-ref stuff a)) (sort (hash-keys stuff) string<?)) ",")]
          [else (for ([(a i) (in-hash stuff)] #:unless (string? i))
                  (when (= 1 (set-count i))
                    (hash-set! stuff a (set-first i))
                    (for ([(k v) (in-hash stuff)] #:unless (or (string? v) (eq? k a)))
                      (hash-set! stuff k (set-remove v (set-first i))))))
                (iter)])))
          
(time (part-1))

(time (part-2))
