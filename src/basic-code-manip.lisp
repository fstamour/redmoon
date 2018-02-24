


(defun extract-assignations* (assignation-body)
  (rest assignation-body)
  (loop :for (var form) :on (rest assignation-body) :by #'cddr
        :collect (list var form)))

(defun extract-assignations (form)
  (when (listp form)
    (case (car form)
      ('set (extract-assignations* form))
      ('while (mapcan 'extract-assignations (cddr form)))
      ('if (mapcan 'extract-assignations (cddr form))))))


(defun extract-variable (form)
  (unless form
    (error "Invalid form 'NIL'"))
  (if (atom? form)
      ;; Atom
      (when (var? form) form)
      (case (car form)
        ;; Assignation
        (set
         (loop :for (var value) :on (rest form) :by #'cddr
               ;; TODO check (var? var)
               :collect `(var ,@(extract-variable value))))
        (while (destructuring-bind (test-form body) (rest form)
                 (append (extract-variable test-form)
                         (extract-variable body))))
        (if (destructuring-bind (test-form then-form &optional else-form) (rest form)
              (append (extract-variable test-form)
                      (extract-variable then-form)
                      (when else-form (extract-variable else-form)))))
        ((+ - * / < > =)
         (mapcar 'extract-variable (rest form)))
        ((declare meta assert ensure)) ;; TODO remove ensure
        (t
         (if (atom? (car form))
             ;; Function call
             (error "Not implemented yet...")
             (mapcar 'extract-variable (rest form)))))))


(defun free-variables (form)
  ;; free = variable not in the lhs of an assignation.
  (flatten form))


