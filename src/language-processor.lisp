
(defpackage :redmoon.core.macros
  (:use :cl :alexandria)
  (:import-from :redmoon
                #:def
                #:while
                #:atom?
                #:var?)
  (:export #:define-processor))

(in-package :redmoon.core.macros)

(defun wrap-in-max-depth (max-depth depth body)
  "Helper function for writing macros.
If max-depth is nil, just return body.
Else, increment depth and check if its greater that max-depth."
  (check-type max-depth (or symbol number))
  (check-type depth symbol)
  (if max-depth
      `(let ((,depth (1+ ,depth)))
         (if (< ,max-depth ,depth) (error "Max eval depth exceeded."))
         ,body)
      body))

(defun declare-max-depth-variable (max-depth depth)
  (when max-depth
    `(defparameter ,depth 0)))

(defun dispatch (name what)
  `(,(symbolicate name '- what) form environment))

(defun handle-arithmetic-and-comparison (name group-arithmetic
                                         group-comparison
                                         group-arithmetic-and-comparison)
  (remove-if
   #'null
   (list
    (when group-arithmetic
      `((+ - * / mod)
        ,(dispatch name 'arithmetic)))
    (when group-comparison
      `((< > = /= <= >=)
        ,(dispatch name 'comparison)))
    (when group-arithmetic-and-comparison
      `((+ - * / mod < > = /= <= >=)
        ,(dispatch name 'arithmetic-and-comparison))))))

(defun handle-grouping (group-arithmetic
                        group-comparison
                        group-arithmetic-and-comparison)
  (append '(set while if not or and def)
          (when (not (or group-arithmetic-and-comparison
                         group-arithmetic))
            '(+ - * / mod))
          (when (not (or group-arithmetic-and-comparison
                         group-comparison))
            '(< > = /= <= >=))))

(defun declare-root-function (name
                              group-arithmetic
                              group-comparison
                              group-arithmetic-and-comparison)
  (assert (not (and group-arithmetic-and-comparison group-comparison)))
  (assert (not (and group-arithmetic-and-comparison group-arithmetic)))
  (labels ((dispatch-case-for-symbols (symbol-list)
             (loop :for symbol :in symbol-list
                   :collect `(,symbol ,(dispatch name symbol)))))
    `(if (atom? form)
;;; Atom
         ,(dispatch name 'atom)
         (case (car form)
           ,@(dispatch-case-for-symbols
              (handle-grouping group-arithmetic
                               group-comparison
                               group-arithmetic-and-comparison))
           ,@(append
              (handle-arithmetic-and-comparison
               name
               group-arithmetic
               group-comparison
               group-arithmetic-and-comparison)
              `((t
                 (if (var? (car form))
;;; Function call
                     ,(dispatch name 'funcall)
;;; Sequence
                     ,(dispatch name 'seq)))))))))

(defmacro define-processor (name &key
                                   max-depth
                                   if-not-handled
                                   optional-environment-p
                                   group-arithmetic
                                   group-comparison
                                   group-arithmetic-and-comparison)
  (check-type name symbol)
  (with-gensyms (depth)
    `(progn
       ,(declare-max-depth-variable max-depth depth)
       (defun ,name
           ,(if optional-environment-p
               `(form &optional environment)
               `(form environment))
         (unless form
           (error "Invalid form 'NIL'"))
         (or
          ,(wrap-in-max-depth
            max-depth depth
            (declare-root-function name group-arithmetic
                                   group-comparison
                                   group-arithmetic-and-comparison))
          ,if-not-handled)))))

