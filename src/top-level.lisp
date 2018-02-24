
(in-package :redmoon)

(defvar *top-level-environment* (make-env))

(defmacro def (&whole whole var &body body)
  `(eval-def ',whole *top-level-environment*))

(defmacro run (&body form)
  `(eval ',form *top-level-environment*))

