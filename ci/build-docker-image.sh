#!/bin/sh

cd "$(git rev-parse --show-toplevel)"

docker build -t redmoon -f ci/Dockerfile .


