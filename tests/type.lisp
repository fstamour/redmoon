
(in-package :redmoon.test.type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-env* (&body body)
    "Like with-env but add implicit arguments to the last form"
    `(with-env
       ,@(butlast body)
       ;; (break)
       ,(append (first (last (last body)))
                '(redmoon:*top-level-environment* *top-level-constraint*))))
  (defmacro is-type (type &body body)
    `(is eq ,type (with-env ,@body)))
  (defmacro is-type* (type &body body)
    `(is eq ,type (with-env* ,@body)))
  (defmacro check-constraint (var type &body body)
    `(is eq ,type (with-env ,@body (get-constraint ,var *top-level-constraint*)))))

(define-test integer!
  (is-type* :integer (integer! 2))
  (is-type* nil
            (add-constraint 'x :bool *top-level-constraint*)
            (integer! 'x))
  (is-type* :integer (integer! 'x)))

(define-test bool!
  (is-type* :bool (bool! 'true))
  (is-type* :bool (bool! 'false))
  (is-type* :integer (bool! 42)) ;; That's not confusing at all.
  (is-type* nil
    (add-constraint 'x :integer *top-level-constraint*)
    (bool! 'x)))

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

