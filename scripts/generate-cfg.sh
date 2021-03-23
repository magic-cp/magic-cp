#!/usr/bin/env bash

cat << EOF > config.cfg
cf-tool-path = "$PWD/bin"
project-root = "$PWD"
cf-parse-dir = "$PWD/cf/contest"
log-root = "$PWD/logs"
EOF
