#lang racket

(require racket/match)
(require racket/function)

(define (most-kcal l) (apply max (hash-values l)))
(define (add n) (curry + n))

;;; part1
(with-input-from-file "input.txt"
  (λ ()
    (for/foldr ([bags (hash)]
                [elf 1]
                #:result (most-kcal bags))
               ([l (in-lines)])
      (match l
        [(regexp #rx"^$") (values bags (add1 elf))]
        [_ (let ([n (string->number l)])
             (values (hash-update bags elf (add n) 0) elf))]))))
