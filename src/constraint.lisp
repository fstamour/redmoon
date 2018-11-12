
(in-package #:redmoon.type)

(defun make-constraint-set ()
  "Create an object to hold a collection of constraints."
  (make-hash-table))

(defun alias? (constraint)
  "Predicate that returns true if the constraint is of type alias."
  (and (listp constraint)
       (eq :alias (first constraint))))

(defun merge-constraint (c1 c2)
  "Merge two contraints c1 and c2"
  (cond
    ;; Return the first one if they are the same
    ((equalp c1 c2) c1)
    ;; If they're both aliases
    ((and (alias? c1)
          (alias? c2))
     ;; Merge them into one
     `(:alias ,@(sort (append (rest c1) (rest c2)) #'string<)))
    ((alias? c2) (merge-constraint-alias c1 c2))
    ((alias? c1) (merge-constraint-alias c2 c1))
    (t (warn "Don't know how to merge those constraints: ~S and ~S" c1 c2))))

(defun merge-constraint-alias (constraint alias)
  (declare (ignorable alias))
  "Merge two contraints, where one of them is an alias constraint."
  constraint)

(defun get-constraint (var constraint)
  "Get the constraint on a variable."
  (gethash var constraint))

(defun add-constraint% (var type constraint)
  (setf (gethash var constraint) type))

;; ok, so, we want to change this
(defun add-constraint (var type constraint)
  "Add a contraints to a variable, merge the constraints with the exising ones if there are any."
  (aif (gethash var constraint)
       ;; If a constraints already exists for this variable.
       (add-constraint% var (merge-constraint it type) constraint)
       (add-constraint% var type constraint)))

(defun add-alias (var1 var2 constraint)
  "Add a constraint of type alias on two variables."
  (add-constraint var2 (list :alias var1) constraint)
  (add-constraint var1 (list :alias var2) constraint))

(defparameter *constraint* (make-hash-table))
(defmacro defconstraint (name &body body)
  "Defines 2 function <name>! and <name>* and register the name in *constraint*.
Each of these functions needs the environment because they can call typeof.
And typeof needs the environment to get the function definitions."
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
