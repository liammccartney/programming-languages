#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"

   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist null) (aunit) "racketlist->mupllist test::empty list")

   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (aunit)) null "racketlist->mupllist test::empty list")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")

   ;; mlet test
   ;(check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   ;(check-equal?
   ;  (eval-exp (mlet "x" (int 5) (add (int 5) (add (var "x") (int 5)))))
   ;  (int 15)
   ;  "mlet test")

   ;(check-equal?
   ;  (eval-exp (mlet "x" (int 5) (mlet "y" (int 5) (add (int 5) (add (var "x") (var "y"))))))
   ;  (int 15)
   ;  "mlet test")

   ;; call test
   (check-equal?
     (eval-exp
       (call
         (closure
           '() ; closure-env
           (fun
             #f ; fun-nameopt
             "x" ; fun-formal
             (add
               (var "x") ; add-e1
               (int 7) ; add-e2
               ) ; fun-body
             ) ; closure-fun
           ) ; call-funexp
         (int 1) ; actual
         )
       )
     (int 8)
     "call test")

   (check-equal?
     (eval-exp
       (call
         (closure
           '() ; closure-env
           (fun
             #f ; fun-nameopt
             "x" ; fun-formal
             (add
               (var "x") ; add-e1
               (add
                 (var "x")
                 (int 7) ; add-e2
                 )
               ) ; fun-body
             ) ; closure-fun
           ) ; call-funexp
         (int 5) ; actual
         )
       )
     (int 17)
     "call test 2")

   (check-equal?
     (eval-exp
       (call
         (closure
           '() ; closure-env
           (fun
             #f ; fun-nameopt
             "x" ; fun-formal
             (aunit) ; fun-body
             ) ; closure-fun
           ) ; call-funexp
         (int 5) ; actual
         )
       )
     (aunit)
     "call test 2")

   ;;fst test
    (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   ;;snd test
    (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")

   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")

   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test happy")
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test sad")
   (check-equal? (eval-exp (ifaunit (call
         (closure
           '() ; closure-env
           (fun
             #f ; fun-nameopt
             "x" ; fun-formal
             (aunit) ; fun-body
             ) ; closure-fun
           ) ; call-funexp
         (int 5) ; actual
         ) (int 2) (int 3))) (int 2) "ifaunit test sad")

   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "foo" (int 10))) (var "foo"))) (int 10) "mlet* test")
   ; (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (var "x"))) (var "y"))) (int 10) "mlet* test")

   ;; ifeq test
   ;(check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

   ;; mupl-map test
   ;(check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
   ;              (apair (int 8) (aunit)) "mupl-map test")

   ;; problems 1, 2, and 4 combined test
   ;(check-equal? (mupllist->racketlist
   ;(eval-exp (call (call mupl-mapAddN (int 7))
   ;                (racketlist->mupllist
   ;                 (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
