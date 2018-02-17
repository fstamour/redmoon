(in-package :cl-user)

(defpackage redmoon.test
  (:import-from parachute
                define-test
                is isnt)
  (:import-from cl
                =
                eq
                eql
                equal
                let)
  (:import-from redmoon
                def
                run)
  (:import-from redmoon.user
                oddp pairp
                exp))

