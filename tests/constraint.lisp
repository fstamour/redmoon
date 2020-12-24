
(in-package #:redmoon.type.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-constraint (() &body body)
    `(let ((constraint (make-constraint-set)))
       ,@body)))



(define-test merge-constraint
  (is eq nil (merge-constraint :true :false))
  (is eq nil (merge-constraint :false :true))
  (is eq :true (merge-constraint :true :true))
  (is eq :false (merge-constraint :false :false))
  (is equalp '(:alias x y) (merge-constraint '(:alias x) '(:alias y)))
  (is eq ':integer (merge-constraint '(:alias x) :integer))
  (is eq ':integer (merge-constraint :integer '(:alias x))))



(define-test add-constraint%
  (is eq :bool
      (with-constraint ()
	(get-constraint
	 (redmoon.type::add-constraint% constraint 'x :bool)
	 'x))))

(define-test add-get-constraint
  (is eq :bool
      (with-constraint ()
        (get-constraint
         (add-constraint constraint  'x :bool)
         'x)))
  (is eq :bool
      (with-constraint ()
        (setf constraint (add-constraint constraint 'x :bool))
        (setf constraint (add-constraint constraint 'x :bool))
        (get-constraint constraint 'x)))
  (is eq nil
      (with-constraint ()
        (setf constraint (add-constraint constraint 'x :bool))
        (setf constraint (add-constraint constraint 'x :integer))
        (get-constraint constraint 'x))))

(define-test add-alias
  (is equalp '(x (:alias a))
      (multiple-value-list
       (fset:find 'x (add-alias (make-constraint-set) 'x 'a))))
  (is equalp '(a (:alias x))
      (multiple-value-list
       (fset:find 'a (add-alias (make-constraint-set) 'x 'a)))))

#+WIP
(with-constraint ()
  (setf constraint (add-alias constraint 'x 'a))
  (add-alias constraint 'y 'a))
