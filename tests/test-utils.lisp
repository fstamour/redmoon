
(in-package :redmoon.test)

(defmacro with-context (() &body body)
  "Create an empty context, perhaps with some variables."
  `(let ((context:*context* (context:make)))
     ,@body))

(defmacro is-type (type &body body)
  `(is equalp ,type (with-context () ,@body)))

(defmacro check-constraint (var type &body body)
  `(is eq ,type (with-context () ,@body
                  (get-constraint ,var *top-level-constraint*))))

