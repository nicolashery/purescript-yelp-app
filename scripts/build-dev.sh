#!/usr/bin/env bash

source $PWD/scripts/_env.sh
# Build first so Webpack can resolve requires
pulp build --src-path src/purs
# Launch watch processes in parallel
parallelshell 'pulp -w build --src-path src/purs' 'webpack -w'
