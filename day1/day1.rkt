#lang racket

(require racket/match
         racket/list
         racket/function
         threading)

(define (add n) (curry + n))

(define (parse-kcals)
 (with-input-from-file "input.txt"
   (λ ()
     (for/foldr ([kcals (hash)]
                 [elf 1]
                 #:result (hash-values kcals))
                ([l (in-lines)])
       (match l
         [(regexp #rx"^$") (values kcals (add1 elf))]
         [_ (let ([n (string->number l)])
              (values (hash-update kcals elf (add n) 0) elf))])))))

(let* ([kcals (parse-kcals)]
       [part1 (~>> kcals (apply max))]
       [part2 (~>  kcals (sort >) (take 3) (apply + _))])
  (printf "part1: ~s\npart2: ~s" part1 part2))
