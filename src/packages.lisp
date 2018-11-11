(in-package :cl-user)

(defpackage redmoon.utils
  (:use cl
        alexandria)
  (:export dolist-butlast))

(defpackage redmoon
  (:use cl
        alexandria
        redmoon.utils)
  (:export

   ;; Main interface
   eval
   def
   run

   ;; keywords
   true false
   + - * / mod = /= < > <= >=
   not and or
   set
   while
   if

   ;; Environment
   get-var
   make-env
   copy-env
   *top-level-environment*
   make-constraint-set
   *top-level-constraint*

   ;; Predicates
   assignation? atom? keyword? var?
   bool? integer? function?

   ;; Host-guest type conversions
   to-bool
   truep

   ;; All kind of evaluations
   eval-atom
   eval-set
   eval-if
   eval-while
   map-eval
   eval-not
   eval-or
   eval-and
   eval-seq
   eval-funcall
   eval-def)
  (:shadow eval inspect))

(defpackage :redmoon.type
  (:nicknames type)
  (:use cl
        alexandria
        anaphora
        redmoon
        redmoon.utils)
  (:shadowing-import-from redmoon
                          eval
                          integer?
                          bool?
                          function?)
  (:export #:typeof
           #:*top-level-constraint*
           #:make-constraint-set
           #:merge-constraint
           #:get-constraint
           #:add-constraint
           #:add-alias
           #:integer?
           #:integer!
           #:integer*
           #:bool?
           #:bool!
           #:bool*))

(defpackage redmoon.user
  (:use redmoon)
  (:import-from redmoon
                def
                run
                inspect)
  (:import-from redmoon.type
                typeof)
  (:export oddp evenp
           exp))
