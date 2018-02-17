(in-package :cl-user)

(defpackage mylisp.test
  (:import-from parachute
                define-test
                is isnt)
  (:import-from cl
                =
                eq
                eql
                equal
                let)
  (:import-from mylisp
                def
                run)
  (:import-from mylisp.user
                oddp pairp
                exp))

