#!/bin/sh

set -e

redmoon_root="$(git rev-parse --show-toplevel)"

sbcl --noinform --non-interactive \
    --eval '(push "'"$redmoon_root"'" ql:*local-project-directories*)' \
    --eval '(declaim (optimize (safety 3) (debug 3) (speed 0)))' \
    --eval '(ql:quickload :redmoon.test)' \
    --eval '(asdf:test-system :redmoon)'

