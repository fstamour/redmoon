
(in-package :redmoon.test)

(defmacro with-env (&body body)
  "Return X, env, constraint"
  `(let* ((env (redmoon:make-env))
          (constraint (redmoon:make-constraint))
          (redmoon:*top-level-environment* env)
          (redmoon:*top-level-constraint* constraint))
     (values (progn ,@body)
             ;; (hash-table-plist env)
             ;; (hash-table-plist constraint)
             )))

