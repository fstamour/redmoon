
(in-package redmoon)

;;; Comparison and arithmetic
(check ()
  (results

   ;; TODO Completlty forgot to test #'mod.
   (eval '(mod 10 2)) ; 0
   (eval '(mod 11 3)) ; 2

   (eval '(< x 1) (make-env '(x 0))) ; true
   (eval '(> x 1) (make-env '(x 0))) ; false
   (eval '(= x 1) (make-env '(x 0))) ; false
   (eval '(= x 1) (make-env '(x 1))) ; true
   (eval '(= 1 2)) ; false
   (eval '(= 1 1 1 1))) ; true
  )

;;; Conditional

(check (:name :if)
  (results
   (eval-if '(if true 42 -1))
   (eval-if '(if false 42 -1))
   (eval-if '(if (< 1 0) 42 -1))))

;;; Loop
(check ()
  (let ((env (make-env '(x 5))))
    (eval '(while (> x 0)
            (set x (- x 1)))
          env)
    (hash-table-plist env)))

;;; Definition

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

