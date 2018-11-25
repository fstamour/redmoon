
(in-package #:cl-user)

(uiop:define-package #:redmoon.context.test
    (:mix
     #:redmoon.test
     #:redmoon.context))

(in-package #:redmoon.context.test)

(define-test make
  (parachute:true (make)))

(define-test get-function
  (parachute:false (with-context ()
                     (get-function 'f *context*))))


(with-context ()
  (get-function 'f))
(test* 'get-function)

(define-test set-function
  (is equal '(def f () (+ 1 2)) ;; '(+ 1 2)
      (with-context ()
        (set-function '(def f () (+ 1 2)))
        (get-function 'f))))

(defun test* (spec)
  (with-output-to-string (*standard-output*)
    (test spec)))

(test* 'set-function)

