
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
  (is eq :integer
      (with-env*
        (redmoon:eval '(set x 42))
        (integer! 'x)))
  (is eq :integer
      (with-env*
        (redmoon:eval '(set x true))
        (integer! 'x)))
  (is eq :integer
      (with-env*
        (integer! 'x))))

