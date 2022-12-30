#lang racket

(require racket/match
         racket/list
         racket/function)

(define (add n) (curry + n))

(define (parse-bags)
 (with-input-from-file "input.txt"
   (λ ()
     (for/foldr ([bags (hash)]
                 [elf 1]
                 #:result bags)
                ([l (in-lines)])
       (match l
         [(regexp #rx"^$") (values bags (add1 elf))]
         [_ (let ([n (string->number l)])
              (values (hash-update bags elf (add n) 0) elf))])))))

(let* ([bags (parse-bags)]
       [part1 (apply max (hash-values bags))]
       [part2 (apply + (take (sort (hash-values bags) >) 3))])
  (printf "part1: ~s\npart2: ~s" part1 part2))

