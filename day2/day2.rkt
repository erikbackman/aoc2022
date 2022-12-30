#lang racket

(require racket/match)
(require threading)
(require racket/list)

(define (move->integer str)
  (~> str (string->list) (first) (char->integer)))

(define a-dec (char->integer #\A))
(define x-dec (char->integer #\X))

(define (score str)
  (match str
    [(regexp #rx"^([A-C]) ([X-Z])$" (list _ l r))
     (let* ([opponent (- (move->integer r) x-dec)]
            [player   (- (move->integer l) a-dec)]
            [chosen   (modulo (+ opponent (sub1 player)) 3)])
       (+ 1 chosen (* 3 opponent)))]))

(define part2
  (with-input-from-file "input.txt"
    (λ ()
      (for/fold ([sum 0])
                ([l (in-lines)])
        (+ sum (score l))))))

(printf "part2: ~s" part2)
