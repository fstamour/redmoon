(in-package #:asdf-user)

(defsystem #:redmoon
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (#:alexandria #:anaphora #:checkl)
  :in-order-to ((asdf:test-op (asdf:test-op #:redmoon.test)))
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:file "utils" :depends-on ("packages"))
     (:file "core" :depends-on ("packages" "utils"))
     (:file "top-level" :depends-on ("core"))
     (:file "constraint" :depends-on ("packages"))
     (:file "type" :depends-on ("constraint"))))
   (:module "lib"
    :components
    ((:file "oddp+evenp")
     (:file "exponential")))))
