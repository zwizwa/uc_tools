#!/bin/sh
# FIXME: This should also update the git repository!
[ -z "$1" ] && echo "usage: $0 <.rocspec>" && exit 1
exec luarocks --local build "$1" # --verbose
