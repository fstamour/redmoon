
(defpackage mylisp
  (:use cl
        alexandria)
  (:import-from checkl
                check
                results)
  (:export eval)
  (:shadow eval))

(defpackage mylsip.user
  (:use mylisp))

