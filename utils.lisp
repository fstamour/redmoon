;;;;;;; WIP

(in-package :mylisp.utils)

;; WIP
#+nil
(progn
  (defmacro fn (&body body)
    )

  (fn oddp) => (function (lambda ($0)
                 (oddp $0))))

(defmacro dolist-butlast ((var list) #+nil(&body) body-butlast #+nil(&body) body-last)
  (check-type var symbol)
  (with-gensyms (iterator)
    `(progn
       (do* ((,iterator ,list (cdr ,iterator))
             (,var (car ,iterator) (car ,iterator)))
            ((null (cdr ,iterator)) ,body-last)
         (unless (null (cdr ,iterator)) ,body-butlast)))))


(check (:name :dolist-butlast
        :output-p t)
  (let ((list (loop :for i :below 15 :collect i)))
    (dolist-butlast (el list)
                    (list :butlasts el)
                    (list :last el))))

