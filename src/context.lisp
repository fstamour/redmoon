
(in-package #:cl-user)

(uiop:define-package #:redmoon.context
    (:use :cl :alexandria)
  (:nicknames #:context)
  (:import-from #:fset
                #:lookup
                #:with)
  (:export
   #:add-constructor-hook
   #:defconstructor
   #:defaccessor
   #:defaccessor*
   #:make
   #:*context*
   #:get-function
   #:set-function)
  (:documentation "Extensible context."))

(in-package #:redmoon.context)

(defparameter *constructor-hooks* (fset:map)
  "List of hooks to call when constructing a context.")

(defparameter *context* nil
  "The current context.")

(defun add-constructor-hook (hook-name function)
  "Add a hook to be called when making a context."
  (setf (fset:lookup *constructor-hooks* hook-name) function))

(defmacro defconstructor (name &body body)
  (check-type name symbol)
  `(add-constructor-hook
    ',name
    #'(lambda (,(symbolicate 'context))
        ,@body)))

;; Plugin v.s. Core
;; Function definition is "very core"
;; Variable value is only needed for evaluation.
;; Type inference is another "plugin"
(defun make ()
  "Create an empty context."
  (let ((context (fset:map
                  (:function-definition (fset:map)))))
    (fset:do-map (hook-name hook *constructor-hooks*)
      (declare (ignorable hook-name))
      #+nil
      (progn
        (format t "~&About to call hook ~a~%" hook-name)
        (force-output))
      (setf context
            (funcall hook context)))
    context))

;; TODO Maybe use some setf-expander instead of those ugly macros?

(defmacro defaccessor (name #+nil &optional) ;; Could be extended
  `(defun ,(symbolicate 'context- name)
       (context)
     (fset:lookup context ,(make-keyword name))))

(defmacro defaccessor* (&rest accessor-spec-list)
  "Helper to define functions to access specific parts of the context."
  `(progn
     ,@(loop :for spec :in accessor-spec-list
             :collect
             (if (listp spec)
                 `(defaccessor ,@spec)
                 `(defaccessor ,spec)))))

(defaccessor function-definition)

(defun get-function (name &optional (context *context*))
  "Get a function's definition from the context."
  (fset:lookup (context-function-definition context) name))

(defun set-function (form)
  (destructuring-bind (def name lambda-list &rest body)
      form
    (declare (ignore def))
    (setf *context*
          (with *context*
                :function-definition
                (with (context-function-definition *context*)
                      name
                      (list name lambda-list body))))))

