
(in-package #:redmoon.type)

(defun alias? (constraint)
  "Predicate that returns true if the constraint is of type alias."
  (and (listp constraint)
       (eq :alias (first constraint))))

(defun merge-constraint (c1 c2)
  "Merge two constraints c1 and c2"
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
  "Merge two constraints, where one of them is an alias constraint."
  constraint)

(defun make-constraint-set  ()
  (fset:map))

(context:defconstructor constraint
  (fset:with context :constraint (make-constraint-set)))

(context:defaccessor :constraint)

(defun get-constraint (constraint var)
  (fset:lookup constraint var))

(defun add-constraint% (constraint var type)
  (setf (fset:lookup constraint var) type)
  constraint)

(defun add-constraint (constraint var type)
  "Add a constraints to a variable, merge the constraints with the exising ones if there are any."
  (aif (get-constraint constraint var)
       ;; If a constraints already exists for this variable.
       (add-constraint% constraint var (merge-constraint it type))
       (add-constraint% constraint var type)))

(defun add-alias (constraint var1 var2)
  "Add a constraint of type alias on two variables."
  (alet
      (add-constraint constraint var2 (list :alias var1))
    (add-constraint it var1 (list :alias var2))))

