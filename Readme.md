# Redmoon

<p align="center"><img src="https://travis-ci.org/fstamour/redmoon.svg?branch=master" alt="TravisCI" /></p>

# Dependencies

* quicklisp
* sbcl (other lisp implementations should work, but the shell scripts use sbcl)
* (Optional) rlwrap

# How to use

## Start

```sh
$ rlwrap ./run.sh
[...]
* (in-package :redmoon.user)
* (run (oddp 2))
```

I strongly suggest to use swank:
```sh
* (ql:quickload :swank)
* (swank:create-server :dont-close t)
```
Then connect to the swank server (either with slime or slimv) change the package
to `redmoon.user` and play around.

Here are some (common lisp) functions or macros that are accessible:
* def
* run
* typeof
* inspect

## The language

The language is designed to be very minimal.

* lisp syntax
* imperative
* typed (100% inferred at the moment)
* `if`, `set`, `while`
* + - * / = /= < > <= >=
* Only 2 types
   * boolean (:true :false)
   * integer (lisp's integer, i.e. no limit except the heap)

# Tests

## How to run all the tests

```sh
ci/sbcl-test.sh
```

## How to run one test

```sh
* (require :redmoon.test)
* (in-package :redmoon.core.test)
* (test '/)
        ？ REDMOON.CORE.TEST::/
  0.000 ✔   (is = 3 (eval '(/ 10 3)))
  0.000 ✔   (is = 0 (eval '(/ x 5) (make-env '(x 2))))
  0.006 ✔ REDMOON.CORE.TEST::/

;; Summary:
Passed:     2
Failed:     0
Skipped:    0
#<PLAIN 3 results>
```

