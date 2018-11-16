
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
   '(#:form #:environment ;; #:def
     #:wrap-in-max-depth
     #:declare-max-depth-variable
     #:dispatch
     #:handle-arithmetic-and-comparison
     #:handle-grouping
     #:declare-root-function )))

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

(define-test dispatch
  (is equalp
      '(eval-atom form environment)
       (dispatch 'eval 'atom)))

(define-test handle-arithmetic-and-comparison
  (is eq nil (handle-arithmetic-and-comparison 'a nil nil nil))
  (is equal
      '(((+ - * / mod) (a-arithmetic form environment)))
      (handle-arithmetic-and-comparison 'a t nil nil))
  (is equal
      '(((< > = /= <= >=) (a-comparison form environment)))
      (handle-arithmetic-and-comparison 'a nil t nil))
  (is equal
      '(((+ - * / mod) (a-arithmetic form environment))
        ((< > = /= <= >=) (a-comparison form environment)))
      (handle-arithmetic-and-comparison 'a t t nil)))

(define-test handle-grouping
  (is equal
      '(set redmoon:while if not or and redmoon.core.macros::def + - * / mod < > = /=
        <= >=)
      (handle-grouping nil nil nil))
  (is equal
      (set redmoon:while if not or and def < > = /= <= >=)
      (handle-grouping t nil nil))
  (is equal
      (set redmoon:while if not or and def + - * / mod)
      (handle-grouping nil t nil))
  (is equal
      (handle-grouping t t nil)
      (handle-grouping nil nil t)))

(define-test declare-root-function
  (is equal
      '(if (redmoon:atom? form)
        (eval2-atom form environment)
        (case (car form)
          (set (eval2-set form environment))
          (redmoon:while (eval2-while form environment))
          (if (eval2-if form environment))
          (not (eval2-not form environment))
          (or (eval2-or form environment))
          (and (eval2-and form environment))
          (redmoon.core.macros::def (eval2-def form environment))
          ((+ - * / mod < > = /= <= >=)
           (eval2-arithmetic-and-comparison form environment))
          (t
           (if (redmoon:var? (car form))
               (eval2-funcall form environment)
               (eval2-seq form environment)))))
      (declare-root-function 'eval2 nil nil t)))

;;;; Putting it all together

(defmacro mock-walker-function (name)
  "Create a bunch of functions to be called by the walker."
  `(progn
     ,@(loop :for element :in (append '(set while if not or and def)
                                      '(seq funcall atom))
             :collect
             `(defun ,(symbolicate name '- element) (form environment)
                (format nil "~a-~a" ',name ',element)))))

(defun fake-eval-arithmetic-and-comparison (form environment)
  (symbol-name 'fake-eval-arithmetic-and-comparison))

(mock-walker-function fake-eval)

(define-processor
    fake-eval
    :optional-environment-p t
  :group-arithmetic-and-comparison t
  :max-depth 100)

(define-test define-processor
  (is equalp "fake-eval-while" (fake-eval '(redmoon:while (smth) 42)))
  (is equalp "fake-eval-set" (fake-eval '(set x 2)))
  (is equalp "fake-eval-def" (fake-eval '(redmoon:def x 2)))
  (is equalp "fake-eval-if" (fake-eval '(if x 2)))
  (is equalp "fake-eval-atom" (fake-eval 42))
  (is equalp "fake-eval-atom" (fake-eval :false))
  (is equalp "fake-eval-seq" (fake-eval '(1)))
  (is equalp "fake-eval-arithmetic-and-comparison" (fake-eval '(+ 1)))
  (is equalp "fake-eval-not" (fake-eval '(not :false)))
  (is equalp "fake-eval-and" (fake-eval '(and :false)))
  (is equalp "fake-eval-or" (fake-eval '(or :false)))
  (is equalp "fake-eval-funcall" (fake-eval '(exp x 42))))

