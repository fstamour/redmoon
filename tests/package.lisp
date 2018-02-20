(in-package :cl-user)

(defpackage redmoon.test
  (:import-from parachute
                define-test
                is isnt
                true false
                fail)
  (:import-from cl
                =
                eq
                eql
                equal
                let)
  (:import-from redmoon
                set
                def
                run)
  (:import-from redmoon.user
                oddp pairp
                exp))

