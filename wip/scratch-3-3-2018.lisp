
(in-package redmoon)

(order 1) ; => true

(order '(+ x 1))

(defun order-set (form env))

(defun order-if (form env)
  (+ condition (max then else)))

(defun order-loop (form env)
  'n)

(defun order-def (form env)
  0)

(defun order (form &optional env)
  (unless form
    (error "Invalid form 'NIL'"))
  (let ((*eval-depth* (1+ *eval-depth*)))
    (if (< 100 *eval-depth*) (error "Max eval depth exceeded."))
    (if (atom? form)
;;; Atom
        1
        (case (car form)
;;; Statements
          (set (order-set form env))
          (while (order-loop form env))
          (if (order-if form env))
;;; Boolean operators
          ((not or and) nil)
;;; Arithmetic and comparison
          ((+ - * / mod = /= < > <= >=)
           ;; TODO
           (sum subforms))
;;; Definition
          (def )
          (t
           (if (var? (car form))
;;; Function call
               ;; TODO order-function
               (eval-funcall form env)
;;; Sequence
               ;; TODO That might be very tricky...
               (eval-seq form env)))))))


