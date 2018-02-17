
(defsystem :redmoon.test
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (:redmoon :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :redmoon.test))
  :serial t
  :components ((:module tests
                :components
                ((:file package)
                 (:module lib
                  :components
                  ((:file tests)))))))

