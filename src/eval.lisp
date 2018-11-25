(in-package :cl-user)

(uiop:define-package #:redmoon.eval
    (:mix #:redmoon #:cl)
  (:import-from #:alexandria #:make-keyword)
  (:import-from #:fset #:lookup)
  (:export #:eval
           #:eval-atom
           #:eval-set
           #:eval-if
           #:eval-while
           #:map-eval
           #:eval-not
           #:eval-or
           #:eval-and
           #:eval-seq
           #:eval-funcall
           #:eval-def

           #:make-environment
           #:context-environment
           #:get-variable
           #:set-variable))

(in-package #:redmoon.eval)

(defun make-environment ()
  (fset:map))

(context:defconstructor variable-value
  (fset:with context :environment (make-environment)))

(context:defaccessor :environment)

(defun get-variable (context var)
  "Get the value of a variable from the environment."
  (or (lookup (context-environment context) var)
      (error "Undefined variable ~S" var)))

(defun set-variable (context variable value)
  "Set the value of a variable in the environment."
  (setf (lookup (lookup context :environment) variable) value)
  context)

(defun make-funcall-context (context)
  "Create an context without any variable values."
  (fset:map (fset:$ context)
            (:environment (make-hash-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-atom (context atom)
  "Evalutate an atom (either a value or a variable)."
  (cond
    ((numberp atom)
     (if (integerp atom)
         atom
         (error "Non-integer numbers are not supported")))
    ((bool? atom) (nth-value 0 (make-keyword atom)))
    ((var? atom)
     (nth-value 0 (get-variable context atom)))))

(defun eval-set (form context)
  "Evalutate an assignement form."
  (loop :for (var value) :on (rest form) :by #'cddr
        ;; TODO check (var? var)
        :do
           ;; (if *trace-assignations*) ;; TODO
           (setf context
                 (set-variable context var (eval context value))))
  context)

(defun eval-if (form context)
  "Eval an if form."
  (destructuring-bind (test-form then-form &optional else-form)
      (rest form)
    (if (truep (eval test-form context))
        (eval then-form context)
        (when else-form
          (eval else-form context)))))

(defun eval-while (form context)
  "Eval a while form."
  (destructuring-bind (condition body)
      (rest form)
    (loop :while (truep (eval condition context))
          :do (eval body context))))

(defun map-eval (form-list context)
  "Eval a list of form and return a list of result."
  (loop :for form :in form-list
        :collect (eval form context)))

(defun eval-not (form context)
  "Eval a form '(not ...)'"
  ;; TODO Check length = 2
  (ecase (eval (second form) context)
    (:true :false)
    (:false :true)))

(defun eval-or (form context)
  "Eval a form '(or ...)'"
  (to-bool (loop :for f :in (rest form)
                 :for truth = (eval f context)
                   :thereis (truep truth))))

(defun eval-and (form context)
  "Eval a form '(and ...)'"
  (to-bool (loop :for f :in (rest form)
                 :for truth = (eval f context)
                 :always (not (truep truth)))))

(defun eval-def (form context)
  "Add a definition to the contextiroment."
  ;; TODO Warn about re-defining something.
  (setf (gethash (second form) context) (cddr form)))

(defun eval-seq (form context)
  "Evaluate a sequence of forms and return the value of the last one.
Just like progn in lisp."
  (loop :for f :on form
        :for v = (eval (car f) context)
        :unless (cdr f) :return v))

(defun eval-funcall(form context)
  "Evaluate a function call."
  (let ((definition (get-function (car form) context)))
    (destructuring-bind (arguments &body body)
        definition
      (eval-seq body
                (let ((new-context (make-funcall-context context)))
                  ;; Then bind each argument to its value
                  (loop :for expression :in (rest form)
                        :for arg :in arguments
                        :for i :from 0 ;; TODO Use this (for better error message for example)
                                       ;; TODO Warn when an arguments shadows a function name.
                        :do (setf (gethash arg new-context)
                                  (eval expression context)))
                  new-context)))))


(defun eval-arithmetic-and-comparison (form context)
  (let ((result (apply (symbol-function (car form))
                       (map-eval (rest form) context))))
    (if (numberp result)
        (nth-value 0 (floor result))
        (to-bool result))))

(redmoon.core.macros:define-processor
    eval
  :group-arithmetic-and-comparison t
  :max-depth 100)
