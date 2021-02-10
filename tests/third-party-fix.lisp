
(in-package :parachute)

(defun parachute:test (designator &rest args &key (report 'plain) &allow-other-keys)
  (let* ((tests (resolve-tests designator))
         (report (apply #'make-instance report
                        :expression designator
                        (removef args :report)))
         (*context* report))
    (dolist (test tests)
      (let ((*package* (home test))) ;; <===== Added this
        (eval-in-context report (result-for-testable test report))))
    (summarize report)))

