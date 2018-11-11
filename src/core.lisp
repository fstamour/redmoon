
(in-package redmoon)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Predicates
;;;;

;;; Symbolics

(defun keyword? (form)
  (and (symbolp form)
       (member (make-keyword form)
               '(:if :set :while
                 :+ :- :* :/ := :/= :< :> :<= :>= :true :false))))

(defun var? (form)
  (unless (or (listp form)
              (keyword? form))
    (symbolp form)))

(defun assignation? (form)
  (and
   (listp form)
   (eq 'set (car form))
   (oddp (length form))))

(defun atom? (form)
  (not (listp form)))

;;; Value Types

(defun integer? (form)
  (integerp form))

(defun bool? (form)
    (and (symbolp form)
     (let ((form (make-keyword form)))
       (or (eq form :true)
           (eq form :false)))))

(defun function? (form)
  (and (listp form)
       (not (assignation? form))
       (if (cdr form) ;; "if there is at least tree elements"
           t
           nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Environments
;;;;

(defun make-env (&optional plist) (plist-hash-table plist))

(defun copy-env (env) (copy-hash-table env))

(defun get-var (var env &optional (error-if-not-found t))
  (let ((x (gethash var env)))
    (if x
        x
        (when error-if-not-found
          (error "Undefined variable ~S" var)))))

(defun make-funcall-env (env)
  "Create an environment with only the function definitons"
  (let ((new-env (copy-hash-table env)))
    (loop :for name :being :the :hash-keys :of env
            :using (hash-value value)
          :when (function? value)
            :do (setf (gethash name new-env) value))
    new-env))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Host-guest type conversions
;;;;

(defun to-bool (b)
  "Convert a generalized boolean to the keyword :true or :false."
  (if b :true :false))

(defun truep (b &optional (error-if-invalid t))
  "Convert the keywords :true or :false to t or nil."
  (let ((b (make-keyword b)))
    (if (eq :true b)
        t
        (unless (eq :false b)
          (when error-if-invalid
            (error "Boolean values must be 'true' or 'false'. Got ~S" b))))))

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
  "Eval an while form."
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


(defparameter *eval-depth* 0
  "Special variable to keep track of the evaluation depth.
Useful when developping, to prevent accidental infinite recursion.")

(defun eval (form &optional (env (make-env)))
  "Evaluate any form, in a new enviroment if none is provided."
  (unless form
    (error "Invalid form 'NIL'"))
  (let ((*eval-depth* (1+ *eval-depth*)))
    (if (< 100 *eval-depth*) (error "Max eval depth exceeded."))
    (if (atom? form)
;;; Atom
        (eval-atom form env)
        (case (car form)
;;; Statements
          (set (eval-set form env))
          (while (eval-while form env))
          (if (eval-if form env))
;;; Boolean operators
          (not (eval-not form env))
          (or (eval-or form env))
          (and (eval-and form env))
;;; Arithmetic and comparison
          ((+ - * / mod = /= < > <= >=)
           (let ((result (apply (symbol-function (car form))
                                (map-eval (rest form) env))))
             (if (numberp result)
                 (nth-value 0 (floor result))
                 (to-bool result))))
;;; Definition
          (def (eval-def form env))
          (t
           (if (var? (car form))
;;; Function call
               (eval-funcall form env)
;;; Sequence
               (eval-seq form env)))))))
