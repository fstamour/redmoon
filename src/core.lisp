
(in-package #:redmoon)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Predicates
;;;;

;;; Symbolics

(defun keyword? (form)
  "Returns true if the form is a keyword of the language."
  (and (symbolp form)
       (member (make-keyword form)
               '(:if :set :while
                 :+ :- :* :/ := :/= :< :> :<= :>= :true :false :def))))

(defun var? (form)
  "Returns true if the form is a symbol but is not a keyword of the language."
  (unless (or (listp form)
              (keyword? form))
    (symbolp form)))

(defun assignation? (form)
  "Returns true if the form is an assignement."
  (and
   (listp form)
   (eq 'set (car form))
   (oddp (length form))))

(defun atom? (form)
  "Returns true if the form is not a list."
  (not (listp form)))

;;; Value Types

(defun integer? (form)
  "Returns true if the form is an integer (same as lisp's integerp)."
  (integerp form))

(defun bool? (form)
  "Returns true if the form is :true or :false."
    (and (symbolp form)
     (let ((form (make-keyword form)))
       (or (eq form :true)
           (eq form :false)))))

(defun function? (form)
  "Returns true if a form as at least 3 elements and is not an assignement form."
  (and (listp form)
       (not (assignation? form))
       (if (cdr form) ;; "if there is at least tree elements"
           t
           nil)))

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

