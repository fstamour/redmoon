
(cl:in-package :redmoon.user)

(def max (x y)
  (if (> x y) x y))

(def min (x y)
  (if (> x y) y x))

(def abs (x)
  (if (< 0 x) (- x) x))

