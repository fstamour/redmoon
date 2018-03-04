
(in-package #:redmoon.test)

(define-test keyword?
  (false (redmoon:keyword? :x))
  (false (redmoon:keyword? 'x))

  (true (redmoon:keyword? :if))
  (true (redmoon:keyword? :set))
  (true (redmoon:keyword? :while))
  (true (redmoon:keyword? :+))
  (true (redmoon:keyword? :-))
  (true (redmoon:keyword? :*))
  (true (redmoon:keyword? :/))
  (true (redmoon:keyword? :=))
  (true (redmoon:keyword? :/=))
  (true (redmoon:keyword? :<))
  (true (redmoon:keyword? :>))
  (true (redmoon:keyword? :<=))
  (true (redmoon:keyword? :>=))
  (true (redmoon:keyword? :true))
  (true (redmoon:keyword? :false))

  (true (redmoon:keyword? 'if))
  (true (redmoon:keyword? 'set))
  (true (redmoon:keyword? 'while))
  (true (redmoon:keyword? '+))
  (true (redmoon:keyword? '-))
  (true (redmoon:keyword? '*))
  (true (redmoon:keyword? '/))
  (true (redmoon:keyword? '=))
  (true (redmoon:keyword? '/=))
  (true (redmoon:keyword? '<))
  (true (redmoon:keyword? '>))
  (true (redmoon:keyword? '<=))
  (true (redmoon:keyword? '>=))
  (true (redmoon:keyword? 'true))
  (true (redmoon:keyword? 'false)))


(define-test var?
  :depends-on (keyword?)
  (true (redmoon:var? 'x)))

(define-test assignation?
  (true (redmoon:assignation? '(set x 42)))
  (true (redmoon:assignation? '(set x 42 y 24)))
  (false (redmoon:assignation? '(set x 42 y)))
  (false (redmoon:assignation? '(not-set bla bla)))
  (false (redmoon:assignation? 'not-even-a-list)))

;; TODO Strings and arrays
(define-test atom?
  (true (redmoon:atom? 42))
  (true (redmoon:atom? 42.5))
  (true (redmoon:atom? 'symbol))
  (true (redmoon:atom? :keyword))
  (false (redmoon:atom? '(some list))))

(define-test integer?
  (true (redmoon:integer? 42))
  (false (redmoon:integer? 42.5))
  (false (redmoon:integer? 'symbol))
  (false (redmoon:integer? :keyword))
  (false (redmoon:integer? '(some list))))

(define-test bool?
  (true (redmoon:bool? :true))
  (true (redmoon:bool? :false))
  (false (redmoon:bool? t))
  (false (redmoon:bool? nil))
  (false (redmoon:bool? 42)))


(define-test function?
  (false (redmoon:function? '(set x 42)))
  (false (redmoon:function? '(set x 42 y 24)))
  ;; (false (redmoon:function? '(set x 42 y)))
  (false (redmoon:function? 42))
  (false (redmoon:function? 42.5))
  (false (redmoon:function? 'symbol))
  (false (redmoon:function? :keyword))
  (false (redmoon:function? 'not-even-a-list))
  (true (redmoon:function? '(this is ok))))

(define-test to-bool
  (is eq :true (redmoon:to-bool t))
  (is eq :true (redmoon:to-bool 42))
  (is eq :false (redmoon:to-bool nil)))

(define-test truep
  (true (redmoon:truep :true))
  (false (redmoon:truep :false))
  ;; Anything else should signal an error.
  (fail (redmoon:truep 42)))

(define-test eval-atom
  (is eq 42 (redmoon:eval-atom 42))
  (fail (redmoon:eval-atom 42.5))
  (is eq :true (redmoon:eval-atom :true))
  (is eq :false (redmoon:eval-atom :false))
  (fail (redmoon:eval-atom 'x))
  (is 'eq 42
      (with-env
        (redmoon:eval-set '(set x 42) env)
        (redmoon:eval-atom 'x env))))

