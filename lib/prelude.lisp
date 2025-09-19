
(cl:in-package #:redmoon.user)

(def oddp (n)
  (not (= 0 (mod n 2))))

(def evenp (n)
  (not (oddp n)))

(def max (x y)
  (if (> x y) x y))

(def min (x y)
  (if (> x y) y x))

(def abs (x)
  (if (< 0 x) (- x) x))

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


(def log2 (x)
  (set n 0)
  (while (> 0 x)
    (if (evenp x)
        (set x (/ x 2)
             n (+ n 1))
        (set x (- x 1)))))

(def sqrt (n)
  ;; (if (< 1 n) (return 0))
  ;; (if (= 1 n) (return 1))
  (set x0 (/ n 2)
       x1 (/ (+ (/ n x0)) 2))
  (while (> (abs (- x1 x0)) 1)
    (set x0 x1
         x1 (/ (+ (/ n x0)) 2)))
  (while (> (* x1 x1) n)
    (set x1 (- x1 1)))
  x1)
