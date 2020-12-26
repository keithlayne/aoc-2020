#lang racket

(define input (port->lines))

(define operator-functions (hash '+ + '- - '* * '/ /))

(define (get-op op)
  (hash-ref operator-functions op))

; This does way more than the problem needs, but I had already written it.
(define (calc add-prec mult-prec exp)
  (struct op (fn arity prec assoc))

  (define tokens
    (map (match-lambda
           [(app string->number (? number? n)) n]
           [(? (match-lambda [(or "(" ")") #t] [_ #f]) p) p]
           [(app string->symbol (? symbol? s)) s])
         (regexp-match* #px"\\d+(?:\\.\\d+)?|[+=*/()]" exp)))

  (define parsed
    (let iter ([input tokens] [output '()])
      (if (null? input)
          (reverse output)
          (match* ((car input) output)
            [('- (or '() (cons (or "(" (? op? _)) _))) (iter (cdr input) (cons (op - 1 2 'right) output))]
            [((? symbol? s) _) (iter (cdr input) (cons (op (get-op s) 2 (if (member s '(* /)) mult-prec add-prec) 'left) output))]
            [(token _) (iter (cdr input) (cons token output))]))))

  (define rpn ; shunting-yard
    (let iter ([input parsed] [output '()] [operators '()])
      (define (eat-ops out oper ops)
        (if (and (not (null? ops))
                 (not (eq? "(" (car ops)))
                 (or (> (op-prec (car ops)) (op-prec oper))
                     (and (= (op-prec (car ops)) (op-prec oper))
                          (eq? (op-assoc oper) 'left))))
            (eat-ops (cons (car ops) out) oper (cdr ops))
            (iter (cdr input) out (cons oper ops))))
      
      (define (match-paren out ops)
        (if (eq? "(" (car ops))
            (iter (cdr input) out (cdr ops))
            (match-paren (cons (car ops) out) (cdr ops))))
      
      (if (null? input)
          (append (reverse output) operators)
          (match (car input)
            [(? number? n) (iter (cdr input) (cons n output) operators)]
            [(? op? oper) (eat-ops output oper operators)]
            ["(" (iter (cdr input) output (cons "(" operators))]
            [")" (match-paren output operators)]))))

  (let compute ([stack '()] [input rpn])
    (match* (stack input)
      [((list result) '()) result]
      [(_ (cons (? number? n) rest)) (compute (cons n stack) rest)]
      [((cons a st) (cons (op fn 1 _ _) rest)) (compute (cons (fn a) st) rest)]
      [((list b a st ...) (cons (op fn 2 _ _) rest)) (compute (cons (fn a b) st) rest)])))

(define (solve add-prec mult-prec)
  (for/sum ([exp (in-list input)])
    (calc add-prec mult-prec exp)))

(define (part-1) (solve 0 0))

(define (part-2) (solve 1 0))

(time (part-1))

(time (part-2))
