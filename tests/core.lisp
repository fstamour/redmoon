(in-package #:redmoon.test.core)

(define-test keyword?
  (false (keyword? :x))
  (false (keyword? 'x))

  (true (keyword? :if))
  (true (keyword? :set))
  (true (keyword? :while))
  (true (keyword? :+))
  (true (keyword? :-))
  (true (keyword? :*))
  (true (keyword? :/))
  (true (keyword? :=))
  (true (keyword? :/=))
  (true (keyword? :<))
  (true (keyword? :>))
  (true (keyword? :<=))
  (true (keyword? :>=))
  (true (keyword? :true))
  (true (keyword? :false))

  (true (keyword? 'if))
  (true (keyword? 'set))
  (true (keyword? 'while))
  (true (keyword? '+))
  (true (keyword? '-))
  (true (keyword? '*))
  (true (keyword? '/))
  (true (keyword? '=))
  (true (keyword? '/=))
  (true (keyword? '<))
  (true (keyword? '>))
  (true (keyword? '<=))
  (true (keyword? '>=))
  (true (keyword? 'true))
  (true (keyword? 'false)))


(define-test var?
  :depends-on (keyword?)
  (true (var? 'x)))

(define-test assignation?
  (true (assignation? '(set x 42)))
  (true (assignation? '(set x 42 y 24)))
  (false (assignation? '(set x 42 y)))
  (false (assignation? '(not-set bla bla)))
  (false (assignation? 'not-even-a-list)))

;; TODO Strings and arrays
(define-test atom?
  (true (atom? 42))
  (true (atom? 42.5))
  (true (atom? 'symbol))
  (true (atom? :keyword))
  (false (atom? '(some list))))

(define-test integer?
  (true (integer? 42))
  (false (integer? 42.5))
  (false (integer? 'symbol))
  (false (integer? :keyword))
  (false (integer? '(some list))))

(define-test bool?
  (true (bool? :true))
  (true (bool? :false))
  (false (bool? t))
  (false (bool? nil))
  (false (bool? 42)))


(define-test function?
  (false (function? '(set x 42)))
  (false (function? '(set x 42 y 24)))
  ;; (false (function? '(set x 42 y)))
  (false (function? 42))
  (false (function? 42.5))
  (false (function? 'symbol))
  (false (function? :keyword))
  (false (function? 'not-even-a-list))
  (true (function? '(this is ok))))

(define-test to-bool
  (is eq :true (to-bool t))
  (is eq :true (to-bool 42))
  (is eq :false (to-bool nil)))

(define-test truep
  (true (truep :true))
  (false (truep :false))
  ;; Anything else should signal an error.
  (fail (truep 42)))

(define-test eval-atom
  (is eq 42 (eval-atom 42))
  (fail (eval-atom 42.5))
  (is eq :true (eval-atom :true))
  (is eq :false (eval-atom :false))
  (fail (eval-atom 'x))
  (is 'eq 42
      (with-env
        (eval-set '(set x 42) *top-level-environment*)
        (eval-atom 'x *top-level-environment*))))

