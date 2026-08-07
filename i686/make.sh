#!/bin/sh
cd $(dirname "$0")

# See https://claude.ai/chat/70d1132d-8cdc-481b-ab52-e273da4c6782
#
# Basic idea:
#
# - Top level make /i/exo/jobserver.mk will invoke us like this:
#   +uc_tools/i686/make.sh
#
# - That '+' indicates that MAKEFLAGS environment variable is set,
#   which contains something like "-j28 --jobserver-auth=fifo:/tmp/GMfifo29453"
#
# - cached-nix-shell will pass that environment variable to the make
#   that runs inside the shell environment
#
# - in case we do not see any MAKEFLAGS, just set them to start a parallel build

# echo "MAKE=$MAKE"
# echo "MAKEFLAGS=$MAKEFLAGS"

if [ -z "$MAKEFLAGS" ]; then
    export MAKEFLAGS="-j$(nproc)"
    # echo "setting MAKEFLAGS=$MAKEFLAGS"
fi
exec cached-nix-shell host.nix --exec make -C . "$@"


