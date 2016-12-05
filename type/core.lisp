
(in-package mylisp.type)
;; (defparameter *types* '(:integer :bool :function ))
;; (deftype)

(defun make-constraint ()
  "Create an object to hold a collection of constraints."
  (make-hash-table))

(defparameter *top-level-constraint*
  (make-constraint)
  "Top level constraints")

;; TODO Add tests for merge-constraint
(defgeneric merge-constraint (c1 c2))

(defmethod merge-constraint (c1 c2)
  (when (equalp c1 c2) c1)
  #+nil (error "Don't know how to merge c1 and c2."))

(defmethod merge-constraint (c1 (c2 (eql :var))) c1)


(defun add-constraint% (var type constraint)
  (setf (gethash var constraint) type))

(defun add-constraint (var type constraint)
  (aif (gethash var constraint)
       ;; If a constraints already exists for this variable.
       (add-constraint% var (merge-constraint it type) constraint)
       (add-constraint% var type constraint)))

(defparameter *constraint* (make-hash-table))
(defmacro defconstraint (name &body body)
  `(progn
     (setf (gethash ',name *constraint*) ',body)
     (defun ,(symbolicate name '?) (form)
       ,@body)
     (defun ,(symbolicate name '!) (form env constraint)
       (if (,(symbolicate name '?) form)
           t
           (if (var? form)
               ;; TODO Check if value of var (in env) passes the constraints.
               (add-constraint form ,(make-keyword name) constraint)
               (typeof form env constraint))))
     (defun ,(symbolicate name '*) (form env constraint)
       ;; (e.g. form == (rest '(+ 1 2 3)))
       (if (every #'(lambda (form)
                      (,(symbolicate name '!) form env constraint))
                  form)
           ,(make-keyword name)
           (list :type-error (format nil "~S" form))))))

(defconstraint integer
    (integerp form))

(defconstraint bool
  (or (eq form 'true)
      (eq form 'false)))

(defun function? (var env)
  (let ((def (get-var var env nil)))
    (if (cdr def) ;; "if there is at least tree elements"
        t
        nil)))

(defun typeof-atom (atom env constraint)
  (cond
    ((bool? atom) :bool)
    ((integer? atom) :integer)
    ((var? atom) (if (function? atom env)
                     (typeof-function atom env constraint)
                     :var))
    ((keyword? atom) :keyword)))

(defun typeof-function (name env constraint)
  (let ((definition (get-var name env))
        (new-constraint (copy-hash-table constraint)))
    (destructuring-bind (arguments &body body)
        definition
      (typeof body env new-constraint)  ; Called for side-effects
      (let* ((arguments-types
               (loop :for arg :in arguments :collect (gethash arg new-constraint)))
             (return-type
               (typeof (last body) env new-constraint)))
        ;; (foreach argument) get type from new-constraint
        `(:function ,@arguments-types ,return-type)))))

(defun typeof-funcall (form env constraint)
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
          :do (add-contraint arg type constraint))

    ;; The type of the function call is the return type of the function.
    (third typeof-function)))

(defun typeof (form  &optional
                       (env mylisp::*top-level-environment*)
                       (constraint *top-level-constraint*))
  (unless form
    (error "Invalid form 'NIL'"))
  (or
   (if (atom? form)
       ;; Atom
       (typeof-atom form env constraint)
       (case (car form)
;;; Statements
         (set :set-statement)     ;; TODO
         (while :while-statement) ;; TODO
         (if (destructuring-bind (test-form then-form &optional else-form)
                 (rest form)
               (if (eq :bool (typeof test-form env constraint))
                   `(:if-statement
                     ,(typeof then-form env constraint)
                     ,@(when else-form
                         (list (typeof then-form env constraint))))
                   `(type-error ,(format nil "Malformed if: ~S" form)))))
;;; Boolean operators
         (not (bool! (second form) env constraint))
         ((or and) (bool* (rest form) env constraint))
;;; Arithmetic
         ((+ - * / mod) (integer* (rest form) env constraint))
;;; Comparison
         ((< > =) (let ((it (integer* (rest form) env constraint)))
                    (if (eq it :integer)
                        :bool
                        it)))
;;; Definition
         (def :def-statement)
         (t
          (if (var? (car form))
;;; Function call
              (typeof-funcall form env constraint)
;;; Sequence
              (loop :for f :in form
                    :for x = (typeof f env constraint)
                    :return x)))))
   (error "Typeof of ~S is nil (this is a bug)." form)))

