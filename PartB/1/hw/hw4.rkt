
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (define aux
    (lambda(low high stride nums)
      (if (or (= low high) (> (+ low stride) high))
          nums
      (aux (+ low stride) high stride (append nums (cons (+ low stride) null))))))
  (if (> high low)
      (aux low high stride (cons low null))
  null))
