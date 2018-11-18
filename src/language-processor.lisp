
(in-package :redmoon.core.macros)

(defun wrap-in-max-depth (max-depth depth body)
  "Helper function for writing macros.
If max-depth is nil, just return body.
Else, increment depth and check if its greater that max-depth."
  (check-type max-depth (or symbol number))
  (check-type depth symbol)
  (if max-depth
      `(let ((,depth (1+ ,depth)))
         (if (< ,max-depth ,depth) (error "Max eval depth exceeded."))
         ,body)
      body))

(defun declare-max-depth-variable (max-depth depth)
  "Generate the form to optionally delcare a variable."
  (when max-depth
    `(defparameter ,depth 0)))

(defun dispatch (name what)
  "Generate the function call with form and environment."
  `(,(symbolicate name '- what) form environment))

(defun handle-arithmetic-and-comparison (walker-spec)
  "Generate \"case\" clauses for arithmetic and comparison with different grouping."
  (with-spec-slot (walker-spec)
    (remove-if
     #'null
     (list
      (when group-arithmetic
        `((+ - * / mod)
          ,(dispatch name 'arithmetic)))
      (when group-comparison
        `((< > = /= <= >=)
          ,(dispatch name 'comparison)))
      (when group-arithmetic-and-comparison
        `((+ - * / mod < > = /= <= >=)
          ,(dispatch name 'arithmetic-and-comparison)))))))

(defun handle-grouping (walker-spec)
  "Generate the list of symbols of that are not handled by the same function (group)."
  (with-spec-slot (walker-spec)
    (append '(set while if not or and def)
            (when (not (or group-arithmetic-and-comparison
                           group-arithmetic))
              '(+ - * / mod))
            (when (not (or group-arithmetic-and-comparison
                           group-comparison))
              '(< > = /= <= >=)))))

(defun auxililry-function-list (walker-spec)
  "Generate a list of symbols"
  (with-spec-slot (walker-spec)
      (let ((not-grouped (handle-grouping walker-spec)))
        (mapcar (curry #'symbolicate name '-)
                (remove-if #'null
                           `(atom funcall seq ,@not-grouped
                                  ,(when group-arithmetic
                                     'arithmetic)
                                  ,(when group-comparison
                                     'comparison)
                                  ,(when group-arithmetic-and-comparison
                                     'arithmetic-and-comparison)))))))

#+nil
((auxililry-function-list
  (make-walker-spec 'a))
 (auxililry-function-list
  (make-walker-spec 'a
                    :group-arithmetic-and-comparison t)))


(defun declare-root-function (walker-spec)
  (with-spec-slot (walker-spec)
    (assert (not (and group-arithmetic-and-comparison group-comparison)))
    (assert (not (and group-arithmetic-and-comparison group-arithmetic)))
    (labels ((dispatch-case-for-symbols (symbol-list)
               (loop :for symbol :in symbol-list
                     :collect `(,symbol ,(dispatch name symbol)))))
      `(if (atom? form)
;;; Atom
           ,(dispatch name 'atom)
           (case (car form)
             ,@(dispatch-case-for-symbols (handle-grouping walker-spec))
             ,@(append
                (handle-arithmetic-and-comparison walker-spec)
                `((t
                   (if (var? (car form))
;;; Function call
                       ,(dispatch name 'funcall)
;;; Sequence
                       ,(dispatch name 'seq))))))))))

(defun check-for-undefined-functions (walker-spec)
  (loop :for function :in (auxililry-function-list walker-spec)
        :collect
        `(unless (fboundp ',function)
           (error "Function \"~a\" is not defined." ',function))))

(defmacro define-processor (name &key
                                   max-depth
                                   if-not-handled
                                   optional-environment-p
                                   group-arithmetic
                                   group-comparison
                                   group-arithmetic-and-comparison)
  (check-type name symbol)
  (with-spec (walker-spec)
    (with-gensyms (depth)
      `(progn
         ,@(check-for-undefined-functions walker-spec)
         ,(declare-max-depth-variable max-depth depth)
         (defun ,name
             ,(if optional-environment-p
                  `(form &optional environment)
                  `(form environment))
           (unless form
             (error "Invalid form 'NIL'"))
           (or
            ,(wrap-in-max-depth
              max-depth depth
              (declare-root-function walker-spec))
            ,if-not-handled))))))

