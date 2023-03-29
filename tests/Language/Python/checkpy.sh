#!/usr/bin/env bash

target=$1
shift
deps=$@
ls ${deps} ${target} | entr sh -c "clear && echo ${target} && mypy --strict ${target}"

