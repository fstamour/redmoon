
(defpackage redmoon.utils
  (:use cl
        alexandria)
  (:import-from checkl
                check
                results)
  (:export dolist-butlast))

(defpackage redmoon
  (:use cl
        alexandria
        redmoon.utils)
  (:import-from checkl
                check
                results)
  (:export eval
           def
           run

           true false
           + - * / mod < > =
           not and or
           set
           while
           if

           get-var
           make-env
           copy-env
           *top-level-environment*
           *top-level-constraint*
           assignation? atom? keyword? var?
           bool? integer? function?)
  (:shadow eval))

(defpackage :redmoon.type
  (:nicknames type)
  (:use cl
        alexandria
        anaphora
        redmoon
        redmoon.utils)
  (:import-from checkl
                check
                results)
  (:shadowing-import-from redmoon
                          eval
                          integer?
                          bool?
                          function?)
  (:export typeof
           *top-level-constraint*))

(defpackage redmoon.user
  (:use redmoon)
  (:import-from redmoon
                def
                run)
  (:export oddp pairp
           exp))


