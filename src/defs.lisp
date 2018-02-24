;;;; Not used atm.

(defparameter *defs* (make-hash-table))
(defparameter *trace-assignations* nil)

(defmacro def (name &body body)
  `(setf (gethash ',name *defs*) ',body))

(defun getdef (name)
  (gethash name *defs*))

(defun precondition (name)
  (first (getdef name)))

;; (precondition 'exp)

(defun postcondition (name)
  (car (last (getdef name))))

;; (postcondition 'exp)


