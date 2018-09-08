(in-package #:redmoon.test.core)

(define-test keyword?
  (false (keyword? :x))
  (false (keyword? 'x))
  (true (keyword? :if))
  (true (keyword? :set))
  (true (keyword? :while))
  (true (keyword? :+))
  (true (keyword? :-))
  (true (keyword? :*))
  (true (keyword? :/))
  (true (keyword? :=))
  (true (keyword? :/=))
  (true (keyword? :<))
  (true (keyword? :>))
  (true (keyword? :<=))
  (true (keyword? :>=))
  (true (keyword? :true))
  (true (keyword? :false))
  (true (keyword? 'if))
  (true (keyword? 'set))
  (true (keyword? 'while))
  (true (keyword? '+))
  (true (keyword? '-))
  (true (keyword? '*))
  (true (keyword? '/))
  (true (keyword? '=))
  (true (keyword? '/=))
  (true (keyword? '<))
  (true (keyword? '>))
  (true (keyword? '<=))
  (true (keyword? '>=))
  (true (keyword? 'true))
  (true (keyword? 'false)))

(define-test var?
  :depends-on (keyword?)
  (true (var? 'x))
  (false (var? 'true))
  (false (var? 'false))
  (false (var? 'if))
  (false (var? '(any thing)))
  (false (var? 'while)))

(define-test assignation?
  (true (assignation? '(set x 42)))
  (true (assignation? '(set x 42 y 24)))
  (false (assignation? '(set x 42 y)))
  (false (assignation? '(not-set bla bla)))
  (false (assignation? 'not-even-a-list)))

;; TODO Strings and arrays
(define-test atom?
  (true (atom? 42))
  (true (atom? 42.5))
  (true (atom? 'symbol))
  (true (atom? 'true))
  (true (atom? 'false))
  (true (atom? :keyword))
  (false (atom? '()))
  (false (atom? '(some list))))

(define-test integer?
  (true (integer? 42))
  (false (integer? 42.5))
  (false (integer? 'symbol))
  (false (integer? :keyword))
  (false (integer? '(some list))))

(define-test bool?
  (true (bool? :true))
  (true (bool? :false))
  (false (bool? t))
  (false (bool? nil))
  (false (bool? 42)))

(define-test function?
  (false (function? '(set x 42)))
  (false (function? '(set x 42 y 24)))
  ;; (false (function? '(set x 42 y)))
  (false (function? 42))
  (false (function? 42.5))
  (false (function? 'symbol))
  (false (function? :keyword))
  (false (function? 'not-even-a-list))
  (true (function? '(this is ok))))

(define-test to-bool
  (is eq :true (to-bool t))
  (is eq :true (to-bool 42))
  (is eq :false (to-bool nil)))

(define-test truep
  (true (truep 'true))
  (false (truep 'false))
  (true (truep :true))
  (false (truep :false))
  ;; Anything else should signal an error.
  (fail (truep 42)))

(define-test eval-atom
  (is = 42 (eval-atom 42))
  (fail (eval-atom 42.5))
  (is eq :true (eval-atom :true))
  (is eq :false (eval-atom :false))
  (fail (eval-atom 'x))
  (is = 42 (eval-atom 'x (make-env '(x 42))))
  (is = 42
      (with-env
        (eval-set '(set x 42) *top-level-environment*)
        (eval-atom 'x *top-level-environment*))))

(define-test eval/atom
  (is = 1 (eval 1))
  (is eq :false (eval :false))
  (is = 42 (eval 'x (make-env '(x 42)))))

(define-test eval/sequence
  (is = 4 (eval '(2 3 4)))
  (is = 3 (eval '((+ 1 2))))
  (is = 4 (eval '((+ x 2)) (make-env '(x 2)))))

(define-test not
  (is eq :false (eval-not '(not :true) (make-env)))
  (is eq :true (eval-not '(not :false) (make-env)))
  (is eq :true (eval-not '(not x) (make-env '(x :false)))))

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
  (is = 42 (eval-if '(if true 42 -1)))
  (is = -1 (eval-if '(if false 42 -1)))
  (is = -1 (eval-if '(if (< 1 0) 42 -1))))

(define-test loop
  (is = 0
      (eval '((while (> i 0)
               (set i (- i 1)))
              i)
            (make-env '(i 5)))))

#+nil
(eval '(def x (a) ((set a (+ 1 a))
                   (* a 2))) env)

