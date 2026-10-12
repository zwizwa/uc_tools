#!/bin/sh
cd "${0%/*}" || exit 1  # shell builtin, so no coreutils needed yet
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
    # nproc is coreutils, which PATH may not have yet.
    export MAKEFLAGS="-j$(nproc 2>/dev/null || echo 4)"
    # echo "setting MAKEFLAGS=$MAKEFLAGS"
fi

using_shell() {
    exec cached-nix-shell host.nix -A shell --exec make -C . "$@"
}

using_env() {
    # Refresh the link only where nix can actually run.  A sandbox or
    # deploy-only context has the /nix/store closure but no nix
    # tooling, and there we just use whatever 'env' already points at.
    if command -v nix-build >/dev/null 2>&1 ; then
        # -L uses the link's timestamp instead of the target
        make -L env
    fi
    # with-env.sh holds the PATH logic, and needs neither nix nor
    # coreutils.  Note it appends to PATH instead of replacing it;
    # replacing it hides the caller's tools (sed, grep, git, ...).
    exec ./with-env.sh make "$@"
}

using_env "$@"
# using_shell "$@"
