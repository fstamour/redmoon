
(cl:in-package :redmoon.user)

(def oddp (n)
  (not (= 0 (mod n 2))))

(def evenp (n)
  (not (oddp n)))
