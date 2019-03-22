
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; 1 sequence
(define (sequence low high stride)
  (define aux
    (lambda (nums low high stride)
      (cond [(or (> low high) (> (+ low stride) high)) nums]
            [(null? nums) (aux (append nums (list low (+ low stride))) (+ low stride) high stride)]
            [(aux (append nums (list (+ low stride))) (+ low stride) high stride)])))
  (aux null low high stride))

; 2 string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3 list-nth-mod
(define (list-nth-mod xs n)
      (cond [(null? xs) error "list-nth-mod: empty list"]
            [(<= n 0) error "list-nth-mod: negative number"]
            [(car (list-tail xs (remainder n (length xs))))]
      ))

; 4 stream-for-n-steps
; It returns a list holding the first n values produced by s in order.
(define (stream-for-n-steps stream n)
  (stream-collector stream n null))

(define (stream-collector stream n xs)
  (let ([next (stream)])
  (cond [(<= n 0) xs]
        [(stream-collector (cdr next) (- n 1) (append xs (list (car next))))])))

(define (funny-number-stream) (list "lol"))
