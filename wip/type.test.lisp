
(in-package redmoon.type)


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

    ;; ,@(redmoon::truth-table 'or 2)
    ;; ,@(redmoon::truth-table 'and 2)
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

