#!/bin/sh

set -e

redmoon_root="$(git rev-parse --show-toplevel)"

sbcl --noinform \
    --eval '(push "'"$redmoon_root"'" ql:*local-project-directories*)' \
    --eval '(ql:quickload :redmoon.test)' \
    --eval "(setf *break-on-signals* 'error)" \
    --eval '(asdf:test-system :redmoon)'

