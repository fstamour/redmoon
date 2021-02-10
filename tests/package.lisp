(in-package :cl-user)

(uiop:define-package :redmoon.test
  (:use :cl)
  (:import-from :parachute
                :define-test
                :is :isnt
                :true :false
                :fail)
  (:import-from :redmoon
                :set
                :def
                :run)
  (:shadowing-import-from :redmoon.user
                :oddp :evenp
                :exp)
  (:export
   :with-env
   :test-all)
  (:reexport
   :parachute
   :redmoon
   :redmoon.user))

(in-package redmoon.test)

(defun test (designator)
  "Run tests for designator and return true if all the tests passed."
  (every #'(lambda (x)
             (eq :passed (parachute:status x)))
         (parachute:results
          (parachute:test designator))))

(defun exit (code &optional dry-run-p)
  (format t "~%Exitig with code ~D" code)
  (unless dry-run-p
    (uiop:quit code)))

(defun test-all (&optional exit-p)
  (or
   (test '(:redmoon.test
           :redmoon.test.core.macros
           :redmoon.test.core
           :redmoon.test.type))
   (exit -1 (not exit-p))))

(uiop:define-package :redmoon.test.core
  (:use :cl :alexandria)
  (:shadowing-import-from :redmoon
   :eval :not :and :or)
  (:use :redmoon)
  (:shadowing-import-from :parachute
   :featurep ;; defined both in alexandria and parachute
   :true :false
   :of-type)
  (:use :parachute)
  (:import-from :redmoon.test
   :with-env)
  (:reexport :cl :alexandria :redmoon))

(uiop:define-package :redmoon.test.type
    (:mix
     :redmoon.type :redmoon.test
     :alexandria :cl))

