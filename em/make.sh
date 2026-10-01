#!/bin/sh
cd $(dirname "$0")
exec cached-nix-shell host.nix --exec ./make-inner.sh -C . "$@"

