;; (checkl:checkl-store "program-proof.checkl")
;; (checkl:checkl-load "program-proof.checkl")

(in-package mylisp)

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

(check (:name :var?)
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

;;;;;;;;;;;;; TODO Make a list of every forms that we would want to test
;; So we could re-use this list for different function (eval, analise, order, proof...)

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

(check () (mapcar 'eval (truth-table 'or 2)))
(check () (mapcar 'eval (truth-table 'and 2)))

(check ()
  (to-bool 'true)
  (to-bool 'false)
  (to-bool nil)
  (truep 'true)
  (truep 'false))

;;; Comparison and arithmetic
(check ()
  (results
   (eval (+ 1 2))
   (eval (* 2 2))
   (eval '(+ x 1) (make-env '(x 0)))
   (eval '(- x 1) (make-env '(x 0)))
   (eval '(* x 1) (make-env '(x 0)))
   (eval '(/ x 5) (make-env '(x 2)))

   ;; TODO Completlty forgot to test #'mod.

   (eval '(< x 1) (make-env '(x 0)))
   (eval '(> x 1) (make-env '(x 0)))
   (eval '(= x 1) (make-env '(x 0)))
   (eval '(= x 1) (make-env '(x 1)))
   (eval '(= 1 2))
   (eval '(= 1 1 1 1))))

;;; Conditional
(eval-if '(if true 42 -1))
(eval-if '(if false 42 -1))
(eval-if '(if (< 1 0) 42 -1))

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
;;  (eval-fun '(f) (make-env))
;;  (eval-fun '(f) (make-env '(f 0)))
(check ()
  (results
   (eval-fun '(f 2) (make-env '(f ((a) a))))
   (eval-fun '(f 2) (make-env '(f ((a) (+ 1 a)))))))


