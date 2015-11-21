#!/usr/bin/env bash

source $PWD/scripts/_env.sh
nodemon -w output --exec node app.js
