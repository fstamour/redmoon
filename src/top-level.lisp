
(in-package :redmoon)

(defvar *top-level-environment* (make-env))

(defmacro def (&whole whole var &body body)
  "Define a top-level variable or function."
  `(eval-def ',whole *top-level-environment*))

(defmacro run (&body form)
  "Evaluate a form in the current environment."
  `(eval ',form *top-level-environment*))

(defun inspect (symbol)
  "Pretty print a variable or function form the current environment."
  (check-type symbol symbol)
  (pprint `(def ,symbol ,@(gethash symbol *top-level-environment*))))

