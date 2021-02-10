
(in-package redmoon.type)

(defparameter *top-level-constraint*
  (make-constraint-set)
  "Top level constraints")

(defparameter *constraint-type* (make-hash-table)
  "Special variable to keep track of all the type of contraints defined.")

(defmacro defconstraint (name docstring)
  "Defines 2 function <name>! and <name>* and register the name in *constraint-type*.
They are helper functions that bridges the function named <name>? and the type inference.

Why those functions needs the environement, and not just the contraints?
Each of these functions needs the environment because they can call typeof.
And typeof needs the environment to get the function definitions."
  (check-type name symbol)
  (check-type docstring string)
  `(progn
     (setf (gethash ',name *constraint-type*) ',docstring)
     (defun ,(symbolicate name '!) (form env constraint)
       ,(format nil "Put the constraint ~A on the form and propagate." name)
       (if (,(symbolicate name '?) form)
           ,(make-keyword name)
           (if (var? form)
               (add-constraint form ,(make-keyword name) constraint)
               (typeof form env constraint))))
     (defun ,(symbolicate name '*) (form env constraint)
       ,(format nil "Put the constraint ~A on each part of the form and propagate." name)
       ;; (e.g. form == (rest '(+ 1 2 3)))
       (if (every #'(lambda (form)
                      (,(symbolicate name '!) form env constraint))
                  form)
           ,(make-keyword name)
           (list :type-error (format nil "~S" form))))))

(defconstraint integer "see integer?")

(defconstraint bool "see bool?")

(defun make-type-error (format-string &rest args)
  "Create an error.
Actually creates a string, should probably be a condition."
  (apply #'format nil format-string args))

(defun typeof-atom (atom env constraint)
  "Infer the type of an atom. Updates the constraints."
  (cond
   ((bool? atom) :bool)
   ((integer? atom) :integer)
   ((var? atom) (if (function? (get-var atom env nil))
                    (typeof-function atom env constraint)
                  (or (get-constraint atom constraint) (list :alias atom))))
   ((keyword? atom) :keyword)))

(defun typeof-if-statement (form env constraint)
  "Infer the type an if statement (i.e. an if whithin a sequence). Updates the contraints."
  (destructuring-bind (test-form then-form &optional else-form)
      (rest form)
    (if (eq :bool (typeof test-form env constraint))
        `(:if-statement
          ,(typeof then-form env constraint)
          ,@(when else-form
              (list (typeof else-form env constraint))))
        (make-type-error "Malformed if: ~S" form))))

(defun typeof-if-expression (form env constraint)
  "Infer the type an if expression. Updates the contraints."
  (destructuring-bind (test-form then-form else-form)
      (rest form)
    (if (eq :bool (typeof test-form env constraint))
        (or
         (merge-constraint (typeof then-form env constraint)
                           (typeof else-form env constraint))
         (make-type-error "Unable to merge constraints from both side of if-expression"))
        (make-type-error "Malformed if (the condition is not a boolean expression): ~S"
                         form))))

(defun typeof-sequence (form env constraint)
  "Infer the types of every parts of a sequence. Updates the contraints."
  (dolist-butlast (f form)
                  (typeof f env constraint nil)
                  (typeof f env constraint t)))

(defun typeof-function (name env constraint)
  "Infer the type of a function, its arguments and return value. Updates the contraints."
  (let ((definition (get-var name env))
        (new-constraint (copy-hash-table constraint)))
    (destructuring-bind (arguments &body body)
        definition
      ;; body
      (typeof-sequence body env new-constraint) ; Called for side-effects
      (let* ((arguments-types
              ;; arguments
              (loop :for arg :in arguments :collect (get-constraint arg new-constraint)))
             (return-type
              (typeof (lastcar body) env new-constraint)))
        (print (hash-table-plist new-constraint)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        `(:function ,@arguments-types ,return-type)))))

(defun typeof-funcall (form env constraint)
  "Infer the type of the expression passed as arguments to a function. Updates the contraints."
  ;; Compute type for each expression passed as argument in the function call.
  (loop :for expression :in (rest form)
        :do (typeof expression env constraint))

  ;; Compute the type of the function to call.
  (let ((typeof-function
          (typeof-function (first form)
                           env
                           constraint)))

    ;; Adjust the constraint on each arguments based on the function's type.
    (loop :for arg :in (rest form)
          :for type :in (ensure-list (second typeof-function))
          :do (add-constraint arg type constraint))

    ;; The type of the function call is the return type of the function.
    (third typeof-function)))

(defun typeof-assignement (form env constraint)
  "Infer the type of variable being assigned.
The form should look like (set [variable value]*)."
  (loop :for (var value) :on (rest form) :by #'cddr
        :do
           (if (var? value)
               (add-alias var value constraint)
               (add-constraint var (typeof value env constraint) constraint))))

(defun typeof-while-statement (form env constraint)
  "Infer the type of a while statement."
  (destructuring-bind (cond &rest body) form
    (bool! cond env constraint)
    (typeof-sequence body env constraint)))

#+nil (defun typeof-arithmetic-and-comparison ())
#+nil (fmakunbound 'typeof-arithmetic-and-comparison)
#+nil (redmoon.core.macros:define-processor typeof
          :group-arithmetic-and-comparison t)

(defun typeof (form  &optional
                       (env redmoon::*top-level-environment*)
                       (constraint *top-level-constraint*)
                       (expression-p t))
  "Infer the type of a form. Updates the contraints.
It needs the enviroment to get the definitions of existing functions."
  (unless form
    (error "Invalid form 'NIL'"))
  (if (atom? form)
;;; Atom
      (typeof-atom form env constraint)
      (case (car form)
;;; Statements
        (set (typeof-assignement form env constraint))
        (while (typeof-while-statement form env constraint))
        (if (if expression-p
                (typeof-if-expression form env constraint)
                (typeof-if-statement form env constraint)))
;;; Boolean operators
        (not (bool! (second form) env constraint))
        ((or and) (bool* (rest form) env constraint))
;;; Arithmetic
        ((+ - * / mod) (integer* (rest form) env constraint))
;;; Comparison
        ((< > = /= <= >=)
         (let ((it (integer* (rest form) env constraint)))
           (if (eq it :integer)
               :bool
               it)))
;;; Definition
        (def :def-statement)
        (t
         (if (and (var? (car form))
                  (function? (get-var (car form) env))
                  (not (keyword? (car form))))
;;; Function call
             (typeof-funcall form env constraint)
;;; Sequence
             (typeof-sequence form env constraint))))))

