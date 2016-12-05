
(cl:in-package :mylisp.user)

(def oddp (n)
  (not (= 0 (mod n 2))))

(def pairp (n)
  (not (oddp n)))

(run (oddp 2))
(run (oddp 1))

(run (pairp 2))

(type:typeof 'oddp)
;; => (:FUNCTION ((:INTEGER :INTEGER) :INTEGER) :BOOL)

(type:typeof '(oddp 2))
;; => bool

(type:typeof '(oddp n) *top-level-environment* (make-constraint))
;; => bool

(type:typeof 'pairp)
;; => (:FUNCTION :INTEGER :BOOL)

(def exp (a b)
  ;;   (ensure t)
  (set x a
       y b
       z 1)
  (while (not (= y 0))
         (if (odd y)
             (set z (* z x)
                  y (- y 1))
             (set x (* x x)
                  y (/ y 2))))
  ;;(ensure (eq z (exp a b)))
  z)

(def f (x) (+ x 2))
(run (f 2))


(run (exp 2 5))

;; Fast O_O
(cl:time
 (run (exp 2 200)))

(get-var 'exp *to)


