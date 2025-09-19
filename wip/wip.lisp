;; TODO Complex numbers!
;; TODO GCD
;; TODO LCM
;; TODO Factorial
;; TODO [Square] root

(parachute:test 'odd)
(parachute:test 'evenp)
(parachute:test 'typeof)
(parachute:test 'def)


;; Fast O_O
(cl:time
 (run (exp 2 200)))

(get-var 'exp *top-level-environment*)

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

(cl:defmacro let (cl:&body body)
  `(cl:let ((redmoon:*top-level-environment*
              (alexandria:copy-hash-table redmoon:*top-level-environment*)
              (redmoon.type:*top-level-constraint*
               (alexandria:copy-hash-table redmoon.type:*top-level-constraint*))))
     ,@body))
