(in-package :redmoon.test)

(define-test odd
  :serial nil
  (is eq :false (run (oddp 2)))
  (is eq :true (run (oddp 1))))

(define-test pairp
  :serial nil
  (is eq :false (run (pairp 1)))
  (is eq :true (run (pairp 2))))

(define-test exp
  (is = 32 (run (exp 2 5))))

(define-test typeof
  (is equal '(:function :integer :bool) (type:typeof 'oddp))
  (is equal '(:function :integer :bool) (type:typeof 'pairp))
  (is eq :bool (type:typeof '(oddp 2)))
  (is eq :bool (type:typeof '(oddp n) redmoon:*top-level-environment* (type::make-constraint))))

(define-test def
  (with-env
    (is = 4
        (def f (x) (+ x 2))
        (run (f 2)))))

