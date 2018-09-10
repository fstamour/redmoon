
(in-package #:redmoon.test.type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-env* (&body body)
    "Like with-env but add implicit arguments to the last form"
    `(with-env
      ,@(butlast body)
      ;; (break)
      ,(append (first (last (last body)))
               '(redmoon:*top-level-environment* *top-level-constraint*))))
  (defmacro with-constraint (&body body)
    `(let ((*top-level-constraint* (make-constraint-set)))
       ,@body))
  (defmacro is-type (type &body body)
    `(is eq ,type (with-env ,@body)))
  (defmacro is-type* (type &body body)
    `(is eq ,type (with-env* ,@body)))
  (defmacro check-constraint (var type &body body)
    `(is eq ,type (with-constraint (with-env ,@body (get-constraint ,var *top-level-constraint*))))))

(define-test merge-constraint
  (is eq nil (merge-constraint :true :false))
  (is eq nil (merge-constraint :false :true))
  (is eq :true (merge-constraint :true :true))
  (is eq :false (merge-constraint :false :false)))

(defmacro is-constraint (type &body body)
  `(check-constraint
    'x ,type
    (macrolet ((add (type) `(add-constraint 'x ,type *top-level-constraint*)))
      (progn
        ,@body
        (get-constraint 'x *top-level-constraint*)))))

(define-test add-get-constraint
  (is-constraint :bool (add :bool))
  (is-constraint :bool (add :bool) (add :bool))
  (is-constraint nil (add :bool) (add :integer)))
