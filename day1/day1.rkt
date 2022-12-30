#lang racket

(require racket/match)

(define (most-kcal l) (apply max l))
(define (bag-sum _ b) (apply + b))

;;; part1
(with-input-from-file "input.txt"
  (λ ()
    (for/foldr ([acc (hash)]
                [elf 1]
                #:result (most-kcal (hash-map acc bag-sum)))
               ([l (in-lines)])
      (match l
        [(regexp #rx"^$") (values acc (add1 elf))]
        [_
         (let ([n (string->number l)])
           (values (hash-update acc elf (λ (v) (cons n v)) '()) elf))]))))
