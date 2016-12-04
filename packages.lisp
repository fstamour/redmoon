
(defpackage mylisp
  (:use cl
        alexandria)
  (:import-from checkl
                check
                results)
  (:export eval
           def
           run
           + - * / mod < > =
           set
           while)
  (:shadow eval))

(defpackage mylisp.user
  (:use mylisp))

