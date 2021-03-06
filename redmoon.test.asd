
(defsystem :redmoon.test
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (:redmoon :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :redmoon.test :test-all t))
  :serial t
  :components ((:module tests
                :components
                ((:file package)
                 (:file test-utils)
                 (:file core)
                 (:file constraint)
                 (:file type)
                 (:file utils)
                 (:module lib
                  :components
                  ((:file tests)))))))
