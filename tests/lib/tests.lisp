(in-package :redmoon.test)

(define-test odd
  :serial nil
  (is eq :false (run (oddp 2)))
  (is eq :true (run (oddp 1))))

(define-test evenp
  :serial nil
  (is eq :false (run (evenp 1)))
  (is eq :true (run (evenp 2))))

(define-test typeof-odd-even
  (is equal '(:function :integer :bool) (type:typeof 'oddp))
  (is equal '(:function :integer :bool) (type:typeof 'evenp))
  (is eq :bool (type:typeof '(oddp 2)))
  #+nil (is eq :bool (type:typeof '(oddp n) redmoon:*top-level-environment* (type::make-constraint-set))))

(define-test exp
  (is = 32 (run (exp 2 5))))

#+nil ;; TODO Doesn't pass yet
(define-test typeof-exp
  (is equal '(:function :integer :integer :integer) (type:typeof 'exp)))

(define-test def
  (is = 4
      (with-env
        (def f (x) (+ x 2))
        (run (f 2)))))
