(in-package :cl-user)

(defpackage redmoon.test
  (:use cl)
  (:import-from parachute
                define-test
                is isnt
                true false
                fail)
  (:import-from redmoon
                set
                def
                run)
  (:shadowing-import-from redmoon.user
                oddp pairp
                exp)
  (:export
   with-env
   test-all))

(in-package redmoon.test)

(defun test-with-boolean-results (package)
    (every #'(lambda (x)
              (eq :passed (parachute:status x)))
     (parachute:results
      (parachute:test package))))

(defun exit (code)
  #+sbcl (sb-ext:exit :code code))

(defun test-all ()
  (and (test-with-boolean-results 'redmoon.test)
       (test-with-boolean-results 'redmoon.test.core)
       (exit 0))
  (exit 1))

(defpackage redmoon.test.core
  (:use cl alexandria)
  (:shadowing-import-from redmoon
                          eval
                          not and or)
  (:use redmoon)
  (:shadowing-import-from parachute
                          true false
                          of-type)
  (:use parachute)
  (:import-from redmoon.test
                with-env))

