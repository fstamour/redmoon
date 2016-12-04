
(check (:name extract-assignations*)
  (extract-assignations* '(set x 1 b 2)))

(check (:name extract-assignations)
  (result
   (extract-assignations '(while b
                           (set x 1)
                           (set y 2)))
   (extract-assignations '(if b s1 s2))
   (extract-assignations '(if b
                           (set x 1)
                           (while b2 (set x (+ x 2)))))))

;; TODO (check (:name extract-variables) ...)
