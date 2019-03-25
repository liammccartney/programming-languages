
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; 1 sequence
(define (sequence low high stride)
  (define aux
    (lambda (nums low high stride)
      (cond [(or (> low high) (> (+ low stride) high)) nums]
            [(null? nums) (aux (append nums (list low (+ low stride))) (+ low stride) high stride)]
            [#t (aux (append nums (list (+ low stride))) (+ low stride) high stride)])))
  (aux null low high stride))

; 2 string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3 list-nth-mod
(define (list-nth-mod xs n)
        ; Jeez - I wasn't _calling_ the error constructor in solution
      (cond [(null? xs) (error "list-nth-mod: empty list")]
            ; Had <= 0 for submission
            [(< n 0) (error "list-nth-mod: negative number")]
            [#t (car (list-tail xs (remainder n (length xs))))]
      ))

; 4 stream-for-n-steps
; It returns a list holding the first n values produced by s in order.
(define (stream-for-n-steps stream n)
  (stream-collector stream n null))

(define (stream-collector stream n xs)
  (let ([next (stream)])
  (cond [(<= n 0) xs]
        [#t (stream-collector (cdr next) (- n 1) (append xs (list (car next))))])))

; 5 funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (* -1 x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

; 6 dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 2) 0) (cons "dog.jpg" (lambda () (f (+ x 1))))]
                      [#t (cons "dan.jpg" (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

; 7 stream-add-zero
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

; 8 cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (as bs)
                (cond [(and (null? as) (null? bs)) (cons (cons (car xs) (car ys)) (lambda () (f (cdr xs) (cdr ys))))]
                      [(null? as) (cons (cons (car xs) (car bs)) (lambda () (f (cdr xs) (cdr bs))))]
                      [(null? bs) (cons (cons (car as) (car ys)) (lambda () (f (cdr as) (cdr ys))))]
                      [#t (cons (cons (car as) (car bs)) (lambda () (f (cdr as) (cdr bs))))]))])
           (lambda () (f xs ys))))



; 9 vector-assoc
(define (vector-assoc n vec)
    (cond [(= (vector-length vec) 0) #f]
          [(not (pair? (vector-ref vec 0))) (vector-assoc n (vector-drop vec 1))]
          [(equal? (car (vector-ref vec 0)) n) (vector-ref vec 0)]
          [#t (vector-assoc n (vector-drop vec 1))]))

; 10 cached-assoc
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-idx 0]
           [f (lambda (v)
                (cond [(vector-assoc v cache) (vector-assoc v cache)]
                      [(> cache-idx (- (vector-length cache)))
                       (begin
                         (set! cache-idx 0)
                         (define rs (assoc v xs))
                         (vector-set! cache cache-idx rs)
                         rs)]
                      [#t (begin
                            (define rs (assoc v xs))
                            (vector-set! cache cache-idx rs)
                            ; Forgot to increment cache in submitted solution
                            (set! cache-idx (+ cache-idx 1))
                            rs)]))])
    f))
