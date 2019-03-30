;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo") COMPLETE
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17) COMPLETE
(struct add  (e1 e2)  #:transparent)  ;; add two expressions COMPLETE
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4 COMPLETE
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function COMPLETE
(struct call (funexp actual)       #:transparent) ;; function call COMPLETE
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) COMPLETE
(struct apair (e1 e2)     #:transparent) ;; make a new pair COMPLETE
(struct fst  (e)    #:transparent) ;; get first part of a pair COMPLETE
(struct snd  (e)    #:transparent) ;; get second part of a pair COMPLETE
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list COMPLETE
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0 COMPLETE

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1
(define (racketlist->mupllist racketlist)
    (if (null? racketlist) (aunit)
      (apair (car racketlist) (racketlist->mupllist (cdr racketlist)))))

(define (mupllist->racketlist mupllist)
  (if (aunit? mupllist)
    null
    (cons (apair-e1 mupllist) (mupllist->racketlist (apair-e2 mupllist)))))

;; CHANGE (put your solutions here)

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  ;(writeln e)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(int? e)
         e]
        [(closure? e)
         e]
        ; (struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
        [(fun? e)
         (eval-under-env (fun-body e) env)
         ]
        ;(struct call (funexp actual) #:transparent) ;; function call
        ;(struct closure (env fun) #:transparent)
        [(call? e)
         (let ([c (call-funexp e)]
               [argval (call-actual e)])
           (cond [(not(closure? c)) (error "call-funexp not a closure")]
                 [(let ([f (closure-fun c)]
                        [c-env (if (null? (closure-env c)) env (cons (closure-env c) env))])
                    (let ([f-env (if (not (fun-nameopt f)) c-env (cons (cons (fun-nameopt) f) c-env))])
                      (eval-under-env f (cons (cons (fun-formal f) argval) f-env)))
                    )]
                 )
           )
         ]
        ; (struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1) (int 1) (int 0)))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [v3 (eval-under-env (ifgreater-e3 e) env)]
               [v4 (eval-under-env (ifgreater-e4 e) env)])
           (if (> (int-num v1) (int-num v3)) v3 v4))]
        [(mlet? e)
         (writeln e)
         (let ([new-env (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (cons v1 v2))]
        ; (struct fst  (e)    #:transparent) ;; get first part of a pair
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (car v1))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (cdr v1))]
        [(aunit? e)
         e]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-exp e1)) (eval-exp e2) (eval-exp e3)))

(define (mlet* lstlst e2)
  (define (build-expr lets expr)
    (let ([v (car lets)])
        (cond ([(null? lets) expr]
               [#t (build-expr (cdr lets) (cons (mlet (car v) (cdr v)) expr))]))))
  (eval-exp (build-expr lstlst null)))


(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
