#!/bin/sh
# Replace this with something that can run a command in an environment with tools.
# Default implementation uses a script based on cached-nix-shell
if [ -z "$(which esptool.py 2>/dev/null)" ]; then
    # echo "$0: env from exo-shell.sh">&2
    # exec /i/exo/nix/exo-shell.sh --exec "$@"
    exec $(dirname "$0")/../../../nix/exo-shell.sh --exec "$@"
else
    # echo "$0: env ok">&2
    exec "$@"
fi
