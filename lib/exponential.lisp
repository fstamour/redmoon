
(cl:in-package :mylisp.user)

(def exp (a b)
  ;;   (ensure t)
  (set x a
       y b
       z 1)
  (while (not (= y 0))
         (if (oddp y)
             (set z (* z x)
                  y (- y 1))
             (set x (* x x)
                  y (/ y 2))))
  ;;(ensure (eq z (exp a b)))
  z)


