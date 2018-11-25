
(in-package :cl-user)

(uiop:define-package redmoon
  (:mix cl
        alexandria
        redmoon.utils)
  (:use-reexport
   redmoon.symbol
   redmoon.context)
  (:export

   ;; Main interface
   eval
   def
   run
   inspect

   ;; Environment
   get-variable
   get-function-definition

   add-context-construction-hook
   define-context-accessor
   *context*

   ;; Predicates
   assignation? atom? keyword? var?
   bool? integer? function?

   ;; Host-guest type conversions
   to-bool
   truep)
  (:shadow eval inspect))

(uiop:define-package :redmoon.type
    (:nicknames #:type)
  (:use #:redmoon.utils)
  (:mix
   redmoon
   cl
   alexandria
   anaphora)
  (:import-from #:fset
                #:loopkup)
  (:export #:typeof
           #:*context*
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

(uiop:define-package redmoon.user
    (:use #:redmoon)
  (:import-from #:redmoon
                #:def
                #:run
                #:inspect)
  (:import-from #:redmoon.type
                #:typeof)
  (:export #:oddp
           #:evenp
           #:exp))

