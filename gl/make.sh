#!/bin/sh
cd $(dirname "$0")
[ -z "$MAKEFLAGS" ] && export MAKEFLAGS="-j$(nproc)"
exec cached-nix-shell host.nix --exec ./make-inner.sh -C . "$@"
