;;;; common lisp function to interact with redmoon

(in-package :redmoon)

(setf context:*context* (context:make))

(defmacro def (&whole whole var &body body)
  "Define a top-level variable or function."
  `(set-function ',whole))

(defmacro run (&body form)
  "Evaluate a form in the current environment."
  `(eval ',form *context*))

;; TODO WIP
(defun inspect (symbol)
  "Pretty print a variable or function form the current environment."
  (check-type symbol symbol)
  (pprint `(def ,symbol ,@(gethash symbol *context*))))
