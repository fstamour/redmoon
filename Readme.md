# Redmoon

<p align="center"><img src="https://travis-ci.org/fstamour/redmoon.svg?branch=master" alt="TravisCI" /></p>

# Dependencies

* quicklisp
* sbcl (other lisp implementations should work, but the shell scripts use sbcl)
* (Optional) rlwrap

# How to use

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
Then connect to the swank server (either with slime or slimv).

# How to run the tests

```sh
ci/sbcl-test.sh
```

#
