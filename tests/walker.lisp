
(uiop:define-package :redmoon.core.macros.test
    (:use :cl :alexandria :redmoon.test)
  (:use :redmoon.core.macros))

(in-package #:redmoon.core.macros.test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun import-internal (package symbols)
    "Import the list of internal symbols from a package."
    (loop :for sym :in symbols
          :do (import
               (find-symbol (symbol-name sym) package))))

  (import-internal
   '#:redmoon.core.macros
   '(#:form #:context ;; #:def
     #:wrap-in-max-depth
     #:declare-max-depth-variable
     #:dispatch
     #:handle-arithmetic-and-comparison
     #:handle-grouping
     #:declare-root-function
     #:make-walker-spec)))

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
      '(eval-atom context form)
       (dispatch 'eval 'atom)))

(define-test handle-arithmetic-and-comparison
  (is eq nil (handle-arithmetic-and-comparison (make-walker-spec :name 'a)))
  (is equal
      '(((+ - * / mod) (a-arithmetic context form)))
      (handle-arithmetic-and-comparison
       (make-walker-spec :name 'a :group-arithmetic t)))
  (is equal
      '(((< > = /= <= >=) (a-comparison context form)))
      (handle-arithmetic-and-comparison
       (make-walker-spec :name 'a :group-comparison t)))
  (is equal
      '(((+ - * / mod) (a-arithmetic context form))
        ((< > = /= <= >=) (a-comparison context form)))
      (handle-arithmetic-and-comparison
       (make-walker-spec :name 'a
                         :group-arithmetic t
                         :group-comparison t))))

(define-test handle-grouping
  (is equal
      '(set redmoon:while if not or and redmoon.core.macros::def + - * / mod < > = /=
        <= >=)
      (handle-grouping (make-walker-spec)))
  (is equal
      '(set redmoon:while if not or and redmoon.core.macros::def < > = /= <= >=)
      (handle-grouping (make-walker-spec :group-arithmetic t)))
  (is equal
      '(set redmoon:while if not or and redmoon.core.macros::def + - * / mod)
      (handle-grouping (make-walker-spec :group-comparison t)))
  (is equal
      '(set redmoon:while if not or and redmoon:def)
      (handle-grouping (make-walker-spec
                        :group-arithmetic t
                        :group-comparison t)))
  (is equal
      (handle-grouping (make-walker-spec
                             :group-arithmetic t
                             :group-comparison t))
      (handle-grouping (make-walker-spec :group-arithmetic-and-comparison t))))

(define-test declare-root-function
  (is equal
      '(if (redmoon:atom? form)
        (eval2-atom context form)
        (case (car form)
          (set (eval2-set context form))
          (redmoon:while (eval2-while context form))
          (if (eval2-if context form))
          (not (eval2-not context form))
          (or (eval2-or context form))
          (and (eval2-and context form))
          (redmoon.core.macros::def (eval2-def context form))
          ((+ - * / mod < > = /= <= >=)
           (eval2-arithmetic-and-comparison context form))
          (t
           (if (redmoon:var? (car form))
               (eval2-funcall context form)
               (eval2-seq context form)))))
      (declare-root-function
       (make-walker-spec :name 'eval2
                         :group-arithmetic-and-comparison t))))

;;;; Putting it all together

(defmacro mock-walker-function (name)
  "Create a bunch of functions to be called by the walker."
  `(progn
     ,@(loop :for element :in (append '(set while if not or and def)
                                      '(seq funcall atom))
             :collect
             `(defun ,(symbolicate name '- element) (context form)
                (declare (ignorable context)
                         (ignorable form))
                (format nil "~a-~a" ',name ',element)))))

(defun fake-eval-arithmetic-and-comparison (context form)
  (declare (ignorable context)
           (ignorable form))
  (symbol-name 'fake-eval-arithmetic-and-comparison))

(mock-walker-function fake-eval)

(define-processor
    fake-eval
  :group-arithmetic-and-comparison t
  :max-depth 100)

(define-test define-processor
  (is equalp "fake-eval-while" (fake-eval nil '(redmoon:while (smth) 42)))
  (is equalp "fake-eval-set" (fake-eval nil '(set x 2)))
  (is equalp "fake-eval-def" (fake-eval nil '(redmoon:def x 2)))
  (is equalp "fake-eval-if" (fake-eval nil '(if x 2)))
  (is equalp "fake-eval-atom" (fake-eval nil  42))
  (is equalp "fake-eval-atom" (fake-eval nil :false))
  (is equalp "fake-eval-seq" (fake-eval nil '(1)))
  (is equalp "fake-eval-arithmetic-and-comparison" (fake-eval nil '(+ 1)))
  (is equalp "fake-eval-not" (fake-eval nil '(not :false)))
  (is equalp "fake-eval-and" (fake-eval nil '(and :false)))
  (is equalp "fake-eval-or" (fake-eval nil '(or :false)))
  (is equalp "fake-eval-funcall" (fake-eval nil '(exp x 42))))

