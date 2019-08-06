(in-package :cl-user)

(uiop:define-package #:redmoon.eval.test
    (:mix #:redmoon.eval #:cl #:redmoon.test))

(in-package #:redmoon.eval.test)

(define-test set-variable
  (is eq 42
      (with-context ()
        (fset:lookup
         (fset:lookup (set-variable *context* 'x 42)
                      :environment)
         'x))))

(define-test get-variable
  (is eq 42
      (with-context ()
        (get-variable (set-variable *context* 'x 42)
                      'x))))

(define-test eval-atom
  (is = 42 (eval-atom nil 42))
  (fail (eval-atom nil 42.5))
  (is eq :true (eval-atom nil :true))
  (is eq :false (eval-atom nil :false))
  (fail (eval-atom nil 'x))
  (is = 42 (with-context ()
             (eval-atom
              (set-variable *context* 'x 42)
              'x))))


(define-test eval/atom
  (is = 1 (eval nil 1))
  (is eq :false (eval nil :false))
  (is = 42 (with-context ()
             (set-variable *context* 'x 42)
             (eval 'x *context*))))

(define-test eval/sequence
  (is = 4 (eval nil '(2 3 4)))
  (is = 3 (eval nil '((+ 1 2))))
  (is = 4 (with-context (x 2)
            (eval *context* '((+ x 2))))))

(define-test not
  (is eq :false (eval-not nil '(not :true)))
  (is eq :true (eval-not nil '(not :false)))
  (is eq :true (with-context (x :false)
                 (eval-not *context* '(not x)))))

(defun truth-table (op n)
  (apply #'map-product
         (lambda (&rest b) `(,op ,@b))
         (loop :for i :below n :collect '(true false))))

(define-test or
  (is equal '(:true :true :true :false)
      (mapcar 'eval (truth-table 'or 2))))

(define-test and
  (is equal '(:false :false :false :true)
      (mapcar 'eval (truth-table 'and 2))))

(define-test +
  (is = 3 (eval '(+ 1 2)))
  (is = 1 (eval '(+ x 1) (make-env '(x 0)))))

(define-test -
  (is = 0 (eval '(- 1 1)))
  (is = -1 (eval '(- x 1) (make-env '(x 0)))))

(define-test *
  (is = 4 (eval '(* 2 2)))
  (is = 0 (eval '(* x 1) (make-env '(x 0)))))

;; integer division
(define-test /
  (is = 3 (eval '(/ 10 3)))
  (is = 0 (eval '(/ x 5) (make-env '(x 2)))))

(define-test mod
  (is = 0 (eval '(mod 10 2)))
  (is = 2 (eval '(mod 11 3))))

(define-test comparison
  (is eq :true (eval '(< x 1) (make-env '(x 0))))
  (is eq :false (eval '(> x 1) (make-env '(x 0))))
  (is eq :false (eval '(= x 1) (make-env '(x 0))))
  (is eq :true (eval '(= x 1) (make-env '(x 1))))
  (is eq :false (eval '(= 1 2)))
  (is eq :true (eval '(= 1 1 1 1))))

(define-test eval-if
  (is = 42 (eval-if '(if true 42 -1) nil))
  (is = -1 (eval-if '(if false 42 -1) nil))
  (is = -1 (eval-if '(if (< 1 0) 42 -1) nil)))

(define-test loop
  (is = 0
      (eval '((while (> i 0)
               (set i (- i 1)))
              i)
            (make-env '(i 5)))))

#+nil
(eval '(def x (a) ((set a (+ 1 a))
                   (* a 2))) env)
