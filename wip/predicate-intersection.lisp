
(def oddp (n)
  (not (= 0 (mod n 2))))

(def evenp (n)
  (not (oddp n)))

(def div-by-4-p (n)
  (= 0 (mod n 4)))

(map-pred '(oddp evenp div-by-4-p))
=>
(let ((e1 (mod 2 n))
      (e2 (= 0 e1)))
  (list e2
        (not e2)
        (when e2
          (= 0 (mod 4 n)))))

