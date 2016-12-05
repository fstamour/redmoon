
(in-package mylisp.type)



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

