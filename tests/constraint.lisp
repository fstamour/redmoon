
(in-package #:redmoon.test.type)

(defun hash-table-sorted-alist (hash-table)
  (sort (copy-seq (hash-table-alist hash-table)) #'string< :key #'car))

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
    `(is eq ,type (with-env ,@body (get-constraint ,var *top-level-constraint*))))
  (defmacro with-constraint-dump (&body body)
    `(with-env
      ,@body
      (hash-table-sorted-alist *top-level-constraint*))) )



(define-test merge-constraint
  (is eq nil (merge-constraint :true :false))
  (is eq nil (merge-constraint :false :true))
  (is eq :true (merge-constraint :true :true))
  (is eq :false (merge-constraint :false :false))
  (is equalp '(:alias x y) (merge-constraint '(:alias x) '(:alias y))))

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

(define-test add-aliad
  (is equalp '((a . (:alias x)) (x . (:alias a)))
      (with-constraint-dump
       (add-alias 'x 'a *top-level-constraint*)))
  (is equalp '((a . (:alias x y)) (x . (:alias a)) (y . (:alias a)))
      (with-constraint-dump
       (add-alias 'x 'a *top-level-constraint*)
       (add-alias 'y 'a *top-level-constraint*))))
