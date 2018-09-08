#!/bin/sh

set -e

redmoon_root="$(git rev-parse --show-toplevel)"

cd "$redmoon_root"

sbcl --noinform \
    --eval '(push "'"$redmoon_root"'" ql:*local-project-directories*)' \
    --eval '(ql:quickload :redmoon.test)' \
    --eval '(ql:quickload :swank)' \
    --eval '(swank:create-server :dont-close t)'

