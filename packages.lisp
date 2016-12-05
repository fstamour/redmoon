
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
           assignation? atom? keyword? var?)
  (:shadow eval))

(defpackage :mylisp.type
  (:nicknames type)
  (:use cl
        alexandria
        anaphora
        mylisp)
  (:shadowing-import-from mylisp eval)
  (:export typeof))

(defpackage mylisp.user
  (:use mylisp)
  (:import-from mylisp
                run))

