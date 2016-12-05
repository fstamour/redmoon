
(cl:in-package :mylisp.user)

(def oddp (n)
  (not (= 0 (mod n 2))))

(def pairp (n)
  (not (oddp n)))

