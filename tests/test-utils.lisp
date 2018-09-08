
(in-package :redmoon.test)

(defmacro with-env (&body body)
  `(let* ((redmoon:*top-level-environment* (redmoon:make-env))
          (redmoon:*top-level-constraint* (redmoon:make-constraint-set)))
     (values (progn ,@body)
             ;; (hash-table-plist env)
             ;; (hash-table-plist constraint)
             )))

