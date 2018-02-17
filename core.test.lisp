;; (checkl:checkl-store "core.test.checkl")
;; (checkl:checkl-load "core.test.checkl")

(in-package redmoon)

(check (:name :var?)
  (results
   (var? 'x)
   (var? 'true)
   (var? 'false)
   (var? 'if)
   (var? '(any thing))
   (var? 'while)))

(check (:name :assignation?)
  (results
   (assignation? '(set x 2))
   (assignation? '(set))
   (assignation? '(not-set))
   (assignation? '(set x))))

(check (:name :atom?)
  (results
   (atom? '())
   (atom? 3)
   (atom? 'v)
   (atom? 'true)
   (atom? 'false)))

;;; Trace for debugging.
#+nil
(trace eval
       eval-atom
       eval-while
       eval-seq
       eval-fn
       make-env
       get-var)

;;; Atom
(check (:name :eval-atom)
  (results
   (eval-atom 1)
   (eval-atom 2)
   (eval-atom 'true)
   (eval-atom 'false)
   (eval-atom 'x (plist-hash-table '(x 42)))))
;; Should fail: (check () (eval-atom 1.5))

(check (:name :eval/atom)
  (results
   (eval 1)
   (eval 2)
   (eval 'true)
   (eval 'false)
   (eval 'x (plist-hash-table '(x 42)))))

;;; Assignation
(check ()
  (let ((env (make-env)))
    (eval '(set x 1) env)
    (eval '(set y false) env)
    (hash-table-plist env)))

(check ()
  (let ((env (make-env)))
    (eval '(set x 1) env)
    (eval '(set x (- x 1)) env)
    (hash-table-plist env)))

;;; Predicates...

(check (:name :bool?)
  (results
   (bool? 'true)
   (bool? 'false)
   (bool? :true)
   (bool? :false)
   (bool? '())
   (bool? t)))

;;; Sequence

(check ()
  (results
   (eval '(2 3 4) (make-env))
   (eval '((+ 1 2)))
   (eval '((+ x 2)) (make-env '(x 2)))))

;;; Boolean logic

(check ()
  (results
   (eval '(not false))
   (eval '(not true))))

(defun truth-table (op n)
  (apply #'map-product
         (lambda (&rest b) `(,op ,@b))
         (loop :for i :below n :collect '(true false))))

(check (:name :or-2-truth-table) (mapcar 'eval (truth-table 'or 2)))
(check (:name :and-2-truth-table) (mapcar 'eval (truth-table 'and 2)))

(check (:name :bool-conversion)
  (results
   (to-bool 'true)  ;; => :true
   (to-bool 'anything) ;; => :true
   (to-bool 'false) ;; => :true
   (to-bool nil) ;; => :false
   (truep 'true)
   (truep 'false)))

;;; Comparison and arithmetic
(check ()
  (results
   (eval (+ 1 2)) ; 3
   (eval (* 2 2)) ; 4
   (eval '(+ x 1) (make-env '(x 0))) ; 1
   (eval '(- x 1) (make-env '(x 0))) ; -1
   (eval '(* x 1) (make-env '(x 0))) ; 0
   (eval '(/ x 5) (make-env '(x 2))) ; 0

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

(check ()
  (results
   (let ((env (make-env)))
     (eval '(def x (+ 1 2)) env)
     (hash-table-plist env))
   (let ((env (make-env '(x 5))))
     (eval '(def x (+ 1 2)) env)
     (hash-table-plist env))))


;;; "Function call"

;; Should fail:
;;  (eval-funcall '(f) (make-env))
;;  (eval-funcall '(f) (make-env '(f 0)))
(check ()
  (results
   (eval-funcall '(f 2) (make-env '(f ((a) a))))
   (eval-funcall '(f 2) (make-env '(f ((a) (+ 1 a)))))))

