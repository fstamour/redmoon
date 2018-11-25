
(cl:in-package :redmoon.user)

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


(def encode (x)
  (if (< n 0) (- n 1) (+ n 1)))

(def decode (x)
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


