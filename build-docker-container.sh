#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
pushd . >/dev/null

cd "$DIR"

stack install --local-bin-path .

docker build -t toodles .

popd >/dev/null

