
(in-package #:redmoon)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eval
;;;;

;; TODO Remove the (make-env)
(defun eval-atom (atom &optional (env (make-env)))
  "Evalutate an atom (either a value or a variable)."
  (cond
    ((numberp atom)
     (if (integerp atom)
         atom
         (error "Non-integer numbers are not supported")))
    ((bool? atom) (nth-value 0 (make-keyword atom)))
    ((var? atom)
     (nth-value 0 (get-var atom env)))))

(defun eval-set (form env)
  "Evalutate an assignement form."
  (loop :for (var value) :on (rest form) :by #'cddr
        ;; TODO check (var? var)
        :do
           ;; (if *trace-assignations*) ;; TODO
           (setf (gethash var env) (eval value env))))

(defun eval-if (form &optional (env (make-env)))
  "Eval an if form."
  (destructuring-bind (test-form then-form &optional else-form)
      (rest form)
    (if (truep (eval test-form env))
        (eval then-form env)
        (when else-form
          (eval else-form env)))))

(defun eval-while (form &optional (env (make-env)))
  "Eval a while form."
  (destructuring-bind (condition body)
      (rest form)
    (loop :while (truep (eval condition env))
          :do (eval body env))))

(defun map-eval (form-list env)
  "Eval a list of form and return a list of result."
  (loop :for form :in form-list
        :collect (eval form env)))

(defun eval-not (form env)
  "Eval a form '(not ...)'"
  ;; TODO Check length = 2
  (ecase (eval (second form) env)
    (:true :false)
    (:false :true)))

(defun eval-or (form env)
  "Eval a form '(or ...)'"
  (to-bool (loop :for f :in (rest form)
                 :for truth = (eval f env)
                   :thereis (truep truth))))

(defun eval-and (form env)
  "Eval a form '(and ...)'"
  (to-bool (loop :for f :in (rest form)
                 :for truth = (eval f env)
                 :always (not (truep truth)))))

(defun eval-def (form env)
  "Add a definition to the enviroment."
  ;; TODO Warn about re-defining something.
  (setf (gethash (second form) env) (cddr form)))

(defun eval-seq (form env)
  "Evaluate a sequence of forms and return the value of the last one.
Just like progn in lisp."
  (loop :for f :on form
        :for v = (eval (car f) env)
        :unless (cdr f) :return v))

(defun eval-funcall(form env)
  "Evaluate a function call."
  (let ((definition (get-var (car form) env)))
    (destructuring-bind (arguments &body body)
        definition
      (eval-seq body
                (let ((new-env (make-funcall-env env)))
                  ;; Then bind each argument to its value
                  (loop :for expression :in (rest form)
                        :for arg :in arguments
                        :for i :from 0 ;; TODO Use this (for better error message for example)
                                       ;; TODO Warn when an arguments shadows a function name.
                        :do (setf (gethash arg new-env)
                                  (eval expression env)))
                  new-env)))))


(defun eval-arithmetic-and-comparison (form environment)
  (let ((result (apply (symbol-function (car form))
                       (map-eval (rest form) environment))))
    (if (numberp result)
        (nth-value 0 (floor result))
        (to-bool result))))

(redmoon.core.macros:define-processor
    eval
  :optional-environment-p t
  :group-arithmetic-and-comparison t
  :max-depth 100)
