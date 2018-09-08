#!/bin/sh

set -e

redmoon_root=$(git rev-parse --show-toplevel)

sbcl --noinform \
    --eval '(push "'"$redmoon_root"'" ql:*local-project-directories*)' \
    --eval '(ql:quickload :redmoon)' \
    --eval '(require :redmoon)'

