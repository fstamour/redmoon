
(in-package :redmoon.test.type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-env* (&body body)
    "Like with-env but add implicit arguments to the last form"
    `(with-env
       ,@(butlast body)
       ;; (break)
       ,(append (first (last (last body)))
                '(redmoon:*top-level-environment* *top-level-constraint*)))))

(define-test integer!
  (is eq :integer
      (with-env* (integer! 2)))
  (false
      (with-env*
        (add-constraint 'x :bool *top-level-constraint*)
        (integer! 'x)))
  (is eq :integer
      (with-env*
        (integer! 'x))))

(define-test bool!
  (is eq :bool (with-env* (bool! 'true)))
  (is eq :bool (with-env* (bool! 'false)))
  (is eq :integer (with-env* (bool! 42))) ;; That's not confusing at all. 
  (false
      (with-env*
        (add-constraint 'x :integer *top-level-constraint*)
        (bool! 'x))))

