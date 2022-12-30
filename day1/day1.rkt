#lang racket

(require racket/match)
(require racket/function)

(define (most-kcal l) (apply max l))

;;; part1
(with-input-from-file "input.txt"
  (λ ()
    (for/foldr ([acc (hash)]
                [elf 1]
                #:result (most-kcal (hash-values acc)))
               ([l (in-lines)])
      (match l
        [(regexp #rx"^$") (values acc (add1 elf))]
        [_
         (let ([n (string->number l)])
           (values (hash-update acc elf (curry + n) 0) elf))]))))
