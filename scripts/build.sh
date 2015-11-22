#!/usr/bin/env bash

source $PWD/scripts/_env.sh
# Build server PureScript
pulp build --src-path src/purs
# Build client PureScript with dead code elimination
pulp build -O --src-path src/purs --main App.Client.Main --to dist/purescript-bundle.js
# Build client JavaScript with production config
NODE_ENV=production webpack
# Concatenate client PureScript and JavaScript into single bundle
cat dist/purescript-bundle.js dist/javascript-bundle.js > dist/bundle.js
