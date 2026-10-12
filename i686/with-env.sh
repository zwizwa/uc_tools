#!/bin/sh

# Run a command with the toolchain on PATH:
#
#   ./with-env.sh make
#   ./with-env.sh i686-elf-gcc --version
#
# The only thing this needs is the 'env' symlink and the /nix/store
# closure it points at.  No nix tooling is involved, so this also
# works in a sandbox or deploy-only context that has the closure but
# no nix, nix-shell or cached-nix-shell.  Create the link on a machine
# that does have nix with:
#
#   make env            # nix-build host.nix -A env -o env
#
# and ship it along with its closure:
#
#   nix-store -qR ./env                   # the 32 paths to copy
#   nix copy --to ssh://host ./env        # or nix-copy-closure
#
# The link doubles as an indirect gc root, so the closure survives
# nix-collect-garbage on the machine that built it.
#
# Deliberately avoids coreutils (dirname, readlink) and bash, since
# neither is guaranteed before PATH is set: ${0%/*} instead of
# dirname, and we go through the symlink rather than resolving it.
# env/bin provides its own make, bash, sh and coreutils.

cd "${0%/*}" || exit 1

if [ ! -e env/bin ] ; then
    echo "$0: no env/bin." >&2
    echo "$0: run 'make env' on a machine with nix, or copy the closure." >&2
    exit 1
fi

# Append rather than replace, so the caller's own tools stay reachable.
PATH="$PWD/env/bin:$PATH"
export PATH

exec "$@"
