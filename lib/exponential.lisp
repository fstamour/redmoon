
(in-package :mylisp.user)

;;;; First example
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
  z
  )


(time
 (eval (getdef 'exp)
       (make-env '(a 2 b 16
                   odd (not (= 0 (mod $ 2)))))))

