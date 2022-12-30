#lang racket

(require racket/match)
(require racket/function)

(define (add n) (curry + n))

(define (solve f)
 (with-input-from-file "input.txt"
   (λ ()
     (for/foldr ([bags (hash)]
                 [elf 1]
                 #:result (f bags))
                ([l (in-lines)])
       (match l
         [(regexp #rx"^$") (values bags (add1 elf))]
         [_ (let ([n (string->number l)])
              (values (hash-update bags elf (add n) 0) elf))])))))

(define (part1) (solve (λ (l) (apply max (hash-values l)))))
(define (part2) (solve (λ (l) (apply + (take (sort (hash-values l) >) 3)))))

(printf "part1: ~s\npart2: ~s" (part1) (part2))
