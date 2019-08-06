(in-package :cl-user)

(uiop:define-package #:redmoon.test
  (:use :cl :alexandria)
  (:import-from :parachute
                :define-test
                :is :isnt
                :true :false
                :fail)
  (:import-from #:redmoon.context
                #:*context*)
  (:export :with-context
           :test-all)
  (:reexport :cl
             :alexandria
             :parachute)
  (:export #:*context*))

(in-package #:redmoon.test)

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
   (test '(
           ;; :redmoon.test
           :redmoon.core.macros.test
           :redmoon.core.test
           :redmoon.type.test
           ))
   (exit -1 (not exit-p))))

(uiop:define-package :redmoon.type.test
    (:mix
     :redmoon.type :redmoon.test
     :alexandria :cl))

