#!/usr/bin/env bash

source $PWD/scripts/_env.sh
nodemon -w js -w output --exec node js/server.js
