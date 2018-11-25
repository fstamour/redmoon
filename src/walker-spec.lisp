
(defpackage :redmoon.core.macros
  (:use :cl :alexandria)
  (:import-from :redmoon
                #:def
                #:while
                #:atom?
                #:var?)
  (:export #:define-processor))

(in-package :redmoon.core.macros)

(defclass walker-specification ()
  ((name :initform nil)
   (max-depth :initform nil)
   (if-not-handled :initform nil)
   (group-arithmetic :initform nil)
   (group-comparison :initform nil)
   (group-arithmetic-and-comparison :initform nil)))


(defmacro set-slots (instance &rest slot-list)
  (once-only (instance)
    `(prog1 ,instance
       ,@(loop :for slot :in slot-list
               :collect `(setf (slot-value ,instance ',slot) ,slot)))))

(defun make-walker-spec
    (&key name
       max-depth
       if-not-handled
       group-arithmetic
       group-comparison
       group-arithmetic-and-comparison)
  (set-slots (make-instance 'walker-specification)
             name
             max-depth
             if-not-handled
             group-arithmetic
             group-comparison
             group-arithmetic-and-comparison))

(defmacro with-spec ((var) &body body)
  "Introduce a variable of type walker-specification.
Uses a lot of variables from the calling context."
  `(let ((,var (set-slots (make-instance 'walker-specification)
                          name
                          max-depth
                          if-not-handled
                          group-arithmetic
                          group-comparison
                          group-arithmetic-and-comparison)))
     ,@body))

(defmacro with-spec-slot ((var) &body body)
  "Splat the spec's slots."
  `(with-slots (name
                max-depth
                if-not-handled
                group-arithmetic
                group-comparison
                group-arithmetic-and-comparison)
       ,var
     ,@body))

