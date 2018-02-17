
(defsystem :mylisp.test
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (:mylisp :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :mylisp.test))
  :serial t
  :components ((:module tests
                :components
                ((:file package)
                 (:module lib
                  :components
                  ((:file tests)))))))

