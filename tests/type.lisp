
(in-package :redmoon.test.type)

(define-test integer!
  (is-type :integer (integer! 2 *context*))
  (is-type nil
            (add-constraint 'x :bool *top-level-constraint*)
            (integer! 'x *context*))
  (is-type :integer (integer! 'x *context*x)))

(define-test bool!
  (is-type :bool (bool! 'true *context*))
  (is-type :bool (bool! 'false *context*))
  (is-type :integer (bool! 42 *context*)) ;; That's not confusing at all.
  (is-type nil
    (add-constraint 'x :integer *context*)
    (bool! 'x *context*)))

(define-test comparison
  (is-type :bool  (typeof '(< 1 0)))
  (is-type :bool (typeof '(> 1 0)))
  (is-type :bool (typeof '(= 1 0)))
  (is-type :bool (typeof '(/= 1 0)))
  (is-type :bool (typeof '(<= 1 0)))
  (is-type :bool (typeof '(>= 1 0)))
  (check-constraint 'n :integer (typeof '(< n 0)))
  (check-constraint 'n :integer (typeof '(> n 0)))
  (check-constraint 'n :integer (typeof '(= n 0)))
  (check-constraint 'n :integer (typeof '(/= n 0)))
  (check-constraint 'n :integer (typeof '(<= n 0)))
  (check-constraint 'n :integer (typeof '(>= n 0))))

(define-test arithmetic
  (is-type :integer (typeof '(+ 1 0)))
  (is-type :integer (typeof '(- 1 0)))
  (is-type :integer (typeof '(* 1 0)))
  (is-type :integer (typeof '(/ 1 0)))
  (is-type :integer (typeof '(mod 1 0)))
  (check-constraint 'n :integer (typeof '(+ n 0)))
  (check-constraint 'n :integer (typeof '(- n 0)))
  (check-constraint 'n :integer (typeof '(* n 0)))
  (check-constraint 'n :integer (typeof '(/ n 0)))
  (check-constraint 'n :integer (typeof '(mod n 0))))

(define-test if
  (is-type :integer (typeof '(if false 42 -1)))
  (is-type :integer (typeof '(if (< 1 0) 42 -1))))

(define-test not
  (is-type :bool (typeof '(not true)))
  (check-constraint 'b :bool (typeof '(not b))))

(define-test and
  (is-type :bool (typeof '(and true false)))
  (check-constraint 'a :bool (typeof '(and a true))))

(define-test or
  (is-type :bool (typeof '(or true false)))
  (check-constraint 'a :bool (typeof '(or a true))))

(define-test assignement
  (check-constraint 'x :integer (typeof '(set x 2)))
  (check-constraint 'x :bool (typeof '(set x :true)))
  (is-type :bool (typeof '((set x :true) x))))

(define-test assignement-alias
  (check-constraint 'x '(:alias a) (typeof '(set x a)))
  (check-constraint 'x :integer (typeof '((set x a) (= x 0))))
  (check-constraint 'x :bool (typeof '((set x a) (not x)))))

(define-test while-statement
  (is-type :bool (typeof '(while :true :true)))
  (is-type :bool (typeof '(while :false :false)))
  (is-type :integer (typeof '(while :true 1)))
  (is-type :integer (typeof '(while :false 1))))

