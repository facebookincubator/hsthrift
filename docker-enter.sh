#!/usr/bin/env sh

docker build . -t glean
docker run -v $(pwd):/hsthrift -it glean bash
