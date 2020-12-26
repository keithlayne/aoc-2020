#lang racket

(define input (regexp-split #px"\n\n" (current-input-port)))

(define (count-valid . regexes)
  (define (valid? record)
    (for/and ([regex regexes])
      (regexp-match? regex record)))
  (count valid? input))

(define (part-1)
  (count-valid #px"\\bbyr:"
               #px"\\biyr:"
               #px"\\beyr:"
               #px"\\bhgt:"
               #px"\\bhcl:"
               #px"\\becl:"
               #px"\\bpid:"))

(define (part-2)
  (count-valid #px"\\bbyr:(19[2-9][0-9]|200[0-2])\\b"
               #px"\\biyr:(201[0-9]|2020)\\b"
               #px"\\beyr:(202[0-9]|2030)\\b"
               #px"\\bhgt:(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)\\b"
               #px"\\bhcl:(#[a-f0-9]{6})\\b"
               #px"\\becl:(amb|blu|brn|gry|grn|hzl|oth)\\b"
               #px"\\bpid:([0-9]{9})\\b"))

(time (part-1))

(time (part-2))