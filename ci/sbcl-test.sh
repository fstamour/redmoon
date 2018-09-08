#!/bin/sh

set -e

redmoon_root="$(git rev-parse --show-toplevel)"

sbcl --noinform --non-interactive \
    --eval '(push "'"$redmoon_root"'" ql:*local-project-directories*)' \
    --eval '(ql:quickload :redmoon.test)' \
    --eval '(asdf:test-system :redmoon)'

