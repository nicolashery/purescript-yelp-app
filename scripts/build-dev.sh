#!/usr/bin/env bash

source $PWD/scripts/_env.sh
# Build first so Webpack can resolve requires
pulp build
# Launch watch processes in parallel
pulp -w build & webpack -w
