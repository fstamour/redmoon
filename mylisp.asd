
(defsystem :mylisp
  :description "Playing with an almost minimal programming language."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public Domain"
  :depends-on (:alexandria :anaphora :checkl)
  :components
  ((:file packages)
   (:file core :depends-on (packages))
   (:file core.test :depends-on (core))
   (:file top-level :depends-on (core))))

