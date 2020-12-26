#lang racket

(define input (port->lines))

; If you just change the angle you end up with floats
(define (rotate dir mag waypoint)
  (let ([degrees (if (equal? dir "R") (- 360 mag) mag)]
        [x (real-part waypoint)]
        [y (imag-part waypoint)])
    (match degrees
      [90 (make-rectangular (- y) x)]
      [180 (make-rectangular (- x) (- y))]
      [270 (make-rectangular y (- x))])))

(define dirs (hash "N" +i "S" -i "E" 1 "W" -1))

(define (step waypoint pos line move-waypoint?)
  (define (set-waypoint f) (values (f waypoint) pos))
  (define (set-pos f) (values waypoint (f pos)))
  (define move-dir (if move-waypoint? set-waypoint set-pos))
  (let ([dir (substring line 0 1)]
        [mag (string->number (substring line 1))])
    (match dir
      [(or "N" "S" "E" "W") (move-dir (curry + (* mag (hash-ref dirs dir))))]
      [(or "L" "R") (set-waypoint (curry rotate dir mag))]
      ["F" (set-pos (curry + (* waypoint mag)))])))

(define (distance waypoint move-waypoint?)
  (define position
    (for/fold ([wp waypoint] [pos 0] #:result pos)
              ([line input])
      (step wp pos line move-waypoint?)))
  (+ (abs (real-part position)) (abs (imag-part position))))

(define (part-1)
  (distance 1 #f))

(define (part-2)
  (distance 10+i #t))

(time (part-1))

(time (part-2))
