#lang racket

(define directions
  (hash "e" '(1 1 0)
        "w" '(-1 -1 0)
        "ne" '(0 1 1)
        "nw" '(-1 0 1)
        "se" '(1 0 -1)
        "sw" '(0 -1 -1)))

(define (line->tile line)
  (apply map + (map (λ (dir) (hash-ref directions dir))
                    (regexp-match* #px"n[ew]?|s[ew]?|e|w" line))))

(define black-tiles
  (apply set-symmetric-difference
         (map (compose set line->tile)
              (port->lines))))

(define (neighbors tile)
  (for/set ([direction (in-hash-values directions)])
    (map + tile direction)))

(define (count-neighbors tiles tile)
  (for/sum ([neighbor (in-set (neighbors tile))]
            #:when (set-member? tiles neighbor)) 1))

(define (with-neighbors tiles)
  (foldl set-union tiles (map neighbors (set->list tiles))))

(define (part-1) (set-count black-tiles))

(define (part-2 [days 100])
  (let iter ([n days] [tiles black-tiles])
    (if (zero? n)
        (set-count tiles)
        (iter (sub1 n)
              (for*/set ([tile (in-set (with-neighbors tiles))]
                         [neighbors (in-value (count-neighbors tiles tile))]
                         [black? (in-value (set-member? tiles tile))]
                         #:when (or (and black? (<= 1 neighbors 2))
                                    (and (not black?) (= 2 neighbors))))
                tile)))))
                                      
(time (part-1))

(time (part-2))
