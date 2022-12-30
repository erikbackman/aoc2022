#lang racket

(require racket/match
         racket/list
         racket/function
         threading)

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

(let* ([bags (hash-values (parse-bags))]
       [part1 (~>> bags (apply max))]
       [part2 (~>  bags (sort >) (take 3) (apply + _))])
  (printf "part1: ~s\npart2: ~s" part1 part2))
