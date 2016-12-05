
(in-package mylisp.type)

;; TODO (define-condition type-error () )
(defun make-type-error (format-string &rest args)
  (apply #'format nil format-string args))
#+nil
(defun make-type-error (datum expected-type)
  (make-condition
   'type-error :datum datum
               :expected-type expected-type))


(defun typeof-atom (atom env constraint)
  (cond
    ((bool? atom) :bool)
    ((integer? atom) :integer)
    ((var? atom) (if (function? (get-var atom env nil))
                     (typeof-function atom env constraint)
                     :var))
    ((keyword? atom) :keyword)))

;; FIXME Arg... We should differentiate if-statements from if-expressions
;; In if-statement we don't care that the then-form as a different type
;; than then else-form.
;; But for an if-expression, we care.
;; Must the else-form be required for an expression?
;; Because, if not, the return type will either be nil (invalid) of (typeof then-form).
;;
;; NOTE: The only place an "if" is not an expression is inside a sequence.
(defun typeof-if-statement (form env constraint)
  (destructuring-bind (test-form then-form &optional else-form)
      (rest form)
    (if (eq :bool (typeof test-form env constraint))
        `(:if-statement
          ,(typeof then-form env constraint)
          ,@(when else-form
              (list (typeof else-form env constraint))))
        (make-type-error "Malformed if: ~S" form))))

(defun typeof-if-expression (form env constraint)
  (destructuring-bind (test-form then-form else-form)
      (rest form)
    (if (eq :bool (typeof test-form env constraint))
        (or
         (merge-constraint (typeof then-form env constraint)
                           (typeof else-form env constraint))
         (make-type-error "Unable to merge constraints from both side of if-expression"))
        (make-type-error "Malformed if (the condition is not a boolean expression): ~S"
                         form))))

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
          :do (add-constraint arg type constraint))

    ;; The type of the function call is the return type of the function.
    (third typeof-function)))

(defun typeof (form  &optional
                       (env mylisp::*top-level-environment*)
                       (constraint *top-level-constraint*)
                       (expression-p t))
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
         (if (if expression-p
                 (typeof-if-expression form env constraint)
                 (typeof-if-statement form env constraint)))
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
              (dolist-butlast (f form)
                              (typeof f env constraint nil)
                              (typeof f env constraint))
              #+nil(loop :for f :in form
                    :for x = 
                    :return x)))))
   (error "Typeof of ~S is nil (this is a bug)." form)))
