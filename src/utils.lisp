
(in-package :redmoon.utils)

(defmacro dolist-butlast ((var list) body-butlast body-last)
  (check-type var symbol)
  (with-gensyms (iterator)
    `(progn
       (do* ((,iterator ,list (cdr ,iterator))
             (,var (car ,iterator) (car ,iterator)))
            ((null (cdr ,iterator)) ,body-last)
         (unless (null (cdr ,iterator)) ,body-butlast)))))
