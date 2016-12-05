;; TODO Complex numbers!
;; TODO GCD
;; TODO LCM
;; TODO Factorial
;; TODO [Square] root

(cl:in-package :mylisp.user)


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



(def f (x) (+ x 2))
(run (f 2))


(run (exp 2 5))

;; Fast O_O
(cl:time
 (run (exp 2 200)))

(get-var 'exp *to)

;; Make sure everything is untrace'd
(cl:time
 (cl:loop :for i :below 10 :collect (eval `(exp 2 ,i) *top-level-environment*)))

(1 2 4 8 16 32 64 128 256 512)



(def 1+ (x)
  (+ 1 x))

(type:typeof '1+)

(def 1- (x)
  (- x 1))

(type:typeof '1-)


(def dist2 (x y)
  (set x2 (* x x)
       y2 (* y y))
  (+ x2 y2))

(type:typeof 'dist2)

(def max (x y) (if (< x y) y x))
(def min (x y) (if (> x y) y x))

(type:typeof 'max)
(type:typeof 'min)


 


