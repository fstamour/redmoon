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

(defun test-all ()
  (parachute:test 'redmoon.test)
  (parachute:test 'redmoon.test.core))

(defpackage redmoon.test.core
  (:use cl)
  (:shadowing-import-from redmoon
                          eval)
  (:use redmoon)
  (:shadowing-import-from parachute
                          true false)
  (:use parachute)
  (:import-from redmoon.test
                with-env))

