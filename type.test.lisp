
(in-package mylisp.type)

(defmacro with-env (&body body)
  "Return X, env, constraint"
  `(let* ((env (make-env))
          (constraint (make-constraint))
          (*top-level-environment* env)
          (*top-level-constraint* constraint))
     (values (progn ,@body)
             ;; (hash-table-plist env) ;; These test sould not change the environment.
             (hash-table-plist constraint))))

(check (:name :integer!)
  (results
   (with-env (integer! 2 env constraint)) ;; FIXME it returns T instead of :integer
   (with-env
     (eval '(set x 42) env)
     (integer! 'x env constraint))
   (with-env
     (eval '(set x true) env)
     (integer! 'x env constraint))))

;; (with-env (integer! x env constraint)) => Fails "Var X is unbound"


(defparameter *forms*
  `(3
    v
    x true false if while set

    (+ 1 2)
    (* 2 2)
    (+ x 1)
    (- x 1)
    (* x 1)
    (/ x 5)

    (+ x false)

    (< x 1)
    (> x 1)
    (= x 1)
    (= 1 2)
    (= 1 1 1 1)

    (set x 2)
    (set x (- x 1))
    (set x true)

    (2 3 4)
    ((+ 1 2))
    ((+ x 2))

    (not false)
    (not true)

    ;; ,@(mylisp::truth-table 'or 2)
    ;; ,@(mylisp::truth-table 'and 2)
    (and false false)
    (or true true)
    (and 1 2)

    (any thing)

    (if false 42 -1)
    (if (< 1 0) 42 -1)

    (while (> x 0)
      (set x (- x 1)))

    (def x (+ 1 2))
    (def x (a)
      (set a (+ 1 a))
      (* a 2))

    (f 2))
  "List of valid forms used for tests.")

(defparameter *invalid-forms*
  '(1.5
    ()
    ""
    (set)
    (set x)
    (not-set))
  "List of invalid forms used for tests.")


(defun typeof-test (forms)
  (loop :for form :in forms
        :collect
        `(,form ,@(multiple-value-list
                   (with-env (typeof form))))))

;; (typeof-test *forms*)

(check (:name :mod)
  (results
   (with-env
     (def n 0)
     (typeof '(mod n 2)))
   (with-env
     (def n 0)
     #+nil (integer! '(mod n 2) env constraint)
     #+nil (integer* '(mod n 2) env constraint)
     (integer? '(mod n 2))) ;; FIXME This returns nil, should return :integer, (n :integer)
   ))


(check (:name :comparison)
  (with-env
    (typeof '(< 1 0) env constraint)))

(check (:name :if)
  (typeof-test
   (remove-if-not #'(lambda (x) (and (listp x)
                                     (eq 'if (first x))))
                  *forms*)))

(check (:name :function-type)
  (results
   (typeof 'mylisp.user::oddp)
   (typeof 'mylisp.user::pairp)))

(typeof 'mylisp.user::exp)

(defmacro with-oddp-pairp (&body body)
  `(with-env
     (def oddp (n)
       (not (= 0 (mod n 2))))
     (def pairp (n)
       (not (oddp n)))
     (def pairp* (n)
       (= 0 (mod n 2)))
     ,@body))

(with-oddp-pairp
  (typeof 'oddp))

(with-oddp-pairp
  (typeof '(oddp n)))

(with-oddp-pairp
  (typeof-funcall '(oddp n) env constraint))

(with-oddp-pairp
  (typeof '(oddp n)))
