;; (checkl:checkl-store "program-proof.checkl")
;; (checkl:checkl-load "program-proof.checkl")

(in-package mylisp)

;; TODO Add tests for atom?

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

;;; Trace for debugging.
#+nil
(trace eval
       eval-atom
       eval-while
       make-env
       get-var)

;;;;;;;;;;;;; TODO Make a list of every forms that we would want to test
;; So we could re-use this list for different function (eval, analise, order, proof...)

;;; Atom
(check ()
  (results
   (eval-atom 1)
   (eval-atom 2)
   (eval-atom 'true)
   (eval-atom 'false)
   (eval-atom 'x (plist-hash-table '(x 42)))))
;; Should fail: (check () (eval-atom 1.5))

(check ()
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
    (hash-table-plist env)))

(check ()
  (let ((env (make-env)))
    (eval '(set x 1) env)
    (eval '(set x (- x 1)) env)
    (hash-table-plist env)))

;;; TODO Sequence

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
   (eval '(+ x 1) (make-env '(x 0)))
   (eval '(- x 1) (make-env '(x 0)))
   (eval '(* x 1) (make-env '(x 0)))
   (eval '(/ x 5) (make-env '(x 2)))
   (eval '(< x 1) (make-env '(x 0)))
   (eval '(> x 1) (make-env '(x 0)))
   (eval '(= x 1) (make-env '(x 0)))
   (eval '(= x 1) (make-env '(x 1)))
   (eval '(= 1 2))
   (eval '(= 1 1 1 1))

   (eval (+ 1 2))
   (eval (* 2 2))))

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

;;; "Function call"

;; This should fail: (eval '(x asd))
#+nil
(check ()
  (eval '(x (y 4)) (make-env '(x (* 2 y)))))


