
(in-package redmoon.type)

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
     (defun ,(symbolicate name '!) (form env constraint)
       (if (,(symbolicate name '?) form)
           ,(make-keyword name)
           (if (var? form)
               (add-constraint form ,(make-keyword name) constraint)
               (typeof form env constraint))))
     (defun ,(symbolicate name '*) (form env constraint)
       ;; (e.g. form == (rest '(+ 1 2 3)))
       (if (every #'(lambda (form)
                      (,(symbolicate name '!) form env constraint))
                  form)
           ,(make-keyword name)
           (list :type-error (format nil "~S" form))))))

(defconstraint integer)

(defconstraint bool)

