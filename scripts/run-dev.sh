#!/usr/bin/env bash

source $PWD/scripts/_env.sh
nodemon -w src/js -w output --exec node src/js/server.js
