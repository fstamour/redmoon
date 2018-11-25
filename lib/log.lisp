
(cl:in-package :redmoon.user)

(def log2 (x)
  (set n 0)
  (while (> 0 x)
    (if (evenp x)
        (set x (/ x 2)
             n (+ n 1))
        (set x (- x 1)))))

