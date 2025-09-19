
(cl:in-package :redmoon.user)

(def elegant-pair (x y)
  (if (> x y)
      (+ (* x x) x y)
      (+ (* y y) x)))

;; Need to return a tuple.
(def elegant-unpair (p)
  (set sroot (sqrt p)
       p* (* sroot sroot)
       x (- p p*)
       y sroot)
  (if (>= x y)
      (set y (- p p* sroot)
           x sroot))
  #(x y))


(def encode (n)
  (if (< n 0) (- n 1) (+ n 1)))

(def decode (n)
  (if (< n 0) (+ n 1) (- n 1)))

;; Needs 'list' and 'for'.
(def list (int-list init)
  (set result init)
  (for (i int-list)
       (set result (elegant-pair result (encode i))))
  result)

;; Needs destructuring
(def reverse (l)
  (set (x y) (elegant-unpair l) ;; Here
       result 0)
  (while (/= 0 y)
    (set result (list y result)
         (x y) (elegant-unpair x)))  ;; And here
  resutl)
