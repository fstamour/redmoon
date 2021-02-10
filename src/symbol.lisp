
(uiop:define-package #:redmoon.symbol
    (:export
     ;; eval inspect

     true false
     + - * / mod = /= < > <= >=
     not and or
     set def
     while if

     true false)
  (:documentation "Symbols used in the language itself"))

