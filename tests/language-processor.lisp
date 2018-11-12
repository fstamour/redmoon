
(uiop:define-package :redmoon.test.core.macros
    (:use :cl :alexandria)
  (:use :redmoon.core.macros)
  (:shadowing-import-from :parachute
   :of-type)
  (:use :parachute)
  (:import-from :redmoon.test
   :with-env)
  (:reexport :cl :alexandria :redmoon))

(in-package #:redmoon.test.core.macros)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun import-internal (package symbols)
    "Import the list of internal symbols from a package."
    (loop :for sym :in symbols
          :do (import
               (find-symbol (symbol-name sym) package))))

  (import-internal
   '#:redmoon.core.macros
   '(#:wrap-in-max-depth
     #:declare-max-depth-variable)))

(define-test wrap-in-max-depth
  (is equalp '(some body)
      (wrap-in-max-depth nil 'asdf '(some body)))
  (is equalp '(let ((asdf (1+ asdf)))
               (if (< 10 asdf)
                   (error "max eval depth exceeded."))
               (some body))
      (wrap-in-max-depth 10 'asdf '(some body))))

(define-test declare-max-depth-variable
  (is eq nil (declare-max-depth-variable nil 'asdf))
  (is equalp '(defparameter asdf 0) (declare-max-depth-variable 10 'asdf)))

