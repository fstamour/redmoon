#!/bin/sh

set -e

redmoon_root="$(git rev-parse --show-toplevel)"

cd "$redmoon_root"

ci/build-docker-image.sh
docker run redmoon

