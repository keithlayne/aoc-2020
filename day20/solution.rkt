#lang racket

; Ugh. Don't even wanna look at this.

(define input (string-split (string-trim (port->string)) "\n\n"))

(define (lines->vector lines)
  (for/vector ([line (in-list lines)])
    (for/vector ([char (in-string line)])
      char)))

(define tiles
  (for*/hash ([group (in-list input)]
              [lines (in-value (string-split group "\n"))])
    (values (string->number (car (regexp-match #px"\\d+" (car lines))))
            (lines->vector (cdr lines)))))

(define size (integer-sqrt (hash-count tiles)))

(define (vector-column vec col)
  (vector-map (λ (v) (vector-ref v col)) vec))

(define (transpose vec)
  (for/vector ([col (in-range (vector-length (vector-ref vec 0)))])
    (vector-column vec col)))

(define (rotate vec)
  (for/vector ([col (in-range (vector-length vec))])
    (vector-column vec (- (vector-length vec) col 1))))

(define (variants tile)
  (let iter ([last tile] [flipped (transpose tile)] [rot 4] [output '()])
    (if (zero? rot)
        output
        (iter (rotate last) (rotate flipped) (sub1 rot) (cons last (cons flipped output))))))

(define (solve)
  (define (fits? pos board variant)
    (match-let* ([(cons _ above) (hash-ref board (+ pos -i) '(#f . #f))]
                 [(cons _ left) (hash-ref board (sub1 pos) '(#f . #f))])
      (and (or (not above)
               (equal? (vector-ref above (sub1 (vector-length variant))) (vector-ref variant 0)))
           (or (not left)
               (equal? (vector-column left (sub1 (vector-length variant))) (vector-column variant 0))))))

  (define (search unused pos board)
    (cond [(hash-empty? unused) board]
          [(>= (real-part pos) size) (search unused (* (add1 (imag-part pos)) +i) board)]
          [else (for*/first ([(key tile) (in-hash unused)]
                             [variant (in-list (variants tile))]
                             #:when (fits? pos board variant)
                             [rest (in-value (search (hash-remove unused key) (add1 pos) (hash-set board pos (cons key variant))))]
                             #:when rest)
                  rest)]))

  (search tiles 0 (hash)))

(printf "Solving...~n")
(define solution (time (solve)))

(define (remove-border tile)
  (for/vector ([row (in-range 1 (sub1 (vector-length tile)))])
    (vector-copy (vector-ref tile row) 1 (sub1 (vector-length tile)))))

(define (compile-image)
  (define (row-of-rows row n)
    (for/list ([col (in-range size)])
      (vector-ref (remove-border (cdr (hash-ref solution (make-rectangular col row)))) n)))
  (define (rows row)
    (let ([height (- (vector-length (cdr (hash-ref solution (* row +i)))) 2)])
      (for/vector ([n (in-range height)])
        (apply vector-append (row-of-rows row n)))))
  (apply vector-append (map rows (range size))))

;01234567890123456789
;                  #
;#    ##    ##    ###
; #  #  #  #  #  #
(define monster '(18 0+i 5+i 6+i 11+i 12+i 17+i 18+i 19+i 1+2i 4+2i 7+2i 10+2i 13+2i 16+2i))

(define (monster-count image)
  (for*/sum ([variant (in-list (variants image))]
             [row (in-range (- (vector-length image) 3))]
             [col (in-range (- (vector-length (vector-ref image 0)) 20))])
    (define (test coord)
      (char=? #\# (vector-ref (vector-ref variant (+ row (imag-part coord))) (+ col (real-part coord)))))
    (if (andmap test monster) 1 0)))

(define (hashes-count image)
  (for/sum ([row (in-vector image)])
    (vector-count (λ (c) (char=? c #\#)) row)))

(define (roughness image)
  (- (hashes-count image)
     (* (length monster) (monster-count image))))

(define (part-1)
  (define corners (list (make-rectangular 0 0)
                        (make-rectangular 0 (sub1 size))
                        (make-rectangular (sub1 size) 0)
                        (make-rectangular (sub1 size) (sub1 size))))
  (define (get-key pos) (car (hash-ref solution pos)))
  (apply * (map get-key corners)))

(define (part-2) (roughness (compile-image)))

(time (part-1))

(time (part-2))
