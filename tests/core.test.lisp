
(in-package redmoon)

;; Call eval-def
(check ()
  (results
   (let ((env (make-env)))
     (eval-def '(def x (+ 1 2)) env)
     (hash-table-plist env))
   (let ((env (make-env '(x 5))))
     (eval-def '(def x (+ 1 2)) env)
     (hash-table-plist env))))
;; Call eval
(check ()
  (results
   (let ((env (make-env)))
     (eval '(def x (+ 1 2)) env)
     (hash-table-plist env))
   (let ((env (make-env '(x 5))))
     (eval '(def x (+ 1 2)) env)
     (hash-table-plist env))
   (let ((env (make-env)))
     (eval '(def x (a) ((set a (+ 1 a))
                        (* a 2))) env)
     (hash-table-plist env))))


;;; "Function call"

;; Should fail:
;;  (eval-funcall '(f) (make-env))
;;  (eval-funcall '(f) (make-env '(f 0)))
(check ()
  (results
   (eval-funcall '(f 2) (make-env '(f ((a) a))))
   (eval-funcall '(f 2) (make-env '(f ((a) (+ 1 a)))))))

