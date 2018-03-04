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
                exp))

