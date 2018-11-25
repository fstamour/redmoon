
(defsystem :redmoon
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (:alexandria :anaphora :fset)
  :in-order-to ((asdf:test-op (asdf:test-op :redmoon.test)))
  :components
  ((:module src
    :components
    ((:file utils)
     (:file symbol)
     (:file context)
     (:file packages)
     (:file core :depends-on (packages symbol))
     (:file walker-spec)
     (:file walker :depends-on (core walker-spec))
     (:file eval :depends-on (packages core walker))
     (:file top-level :depends-on (core))
     (:file constraint :depends-on (packages))
     (:file type :depends-on (constraint utils))))
   (:module lib
    :components
    ((:file oddp+evenp)
     (:file min+max+abs)
     (:file log)
     (:file exponential)))))

