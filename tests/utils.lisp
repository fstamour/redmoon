
(in-package :redmoon.test)

(define-test dolist-bultast
  (is = 12
      (let ((sum 0)
            (list (loop :for i :below 4 :collect i))) ;; from 0 to 3 inclusively
        (redmoon.utils:dolist-butlast (el list)
                                      (incf sum el)
                                      (incf sum (* 3 el))))))
