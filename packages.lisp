
(defpackage mylisp
  (:use cl
        alexandria)
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
           *top-level-environment*
           assignation? atom? keyword? var?
           bool? integer?)
  (:shadow eval))

(defpackage :mylisp.type
  (:nicknames type)
  (:use cl
        alexandria
        anaphora
        mylisp)
  (:import-from checkl
                check
                results)
  (:shadowing-import-from mylisp
                          eval
                          integer?
                          bool?
                          function?)
  (:export typeof))

(defpackage mylisp.user
  (:use mylisp)
  (:import-from mylisp
                def
                run))

