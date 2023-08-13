#!/bin/sh

NPROC=$(nproc)
cd $(dirname $0)

#EXO_DEV=/nix/store/0lzwzl9ysckfmrn71dhcyp6g9q2474la-exo-dev
EXO_DEV=/nix/store/pa18q4jq3yy037l6g8pz3hq74v2n956f-exo-dev
. $EXO_DEV/env

export PATH=$EXO_DEV_PATH
export LDFLAGS_EXTRA="$EXO_DEV_LDFLAGS"
export CFLAGS_EXTRA="$EXO_DEV_CFLAGS"

exec make -j${NPROC}


# OLD: Use a full description of the build tools and use
# (cached-)nix-shell to build it.

# This is untenable.  Instead, use the 'exo-dev' approach above, which
# standardizes on a single build environment that can build all
# projects integrated in the exo/redo build.

# 1. Do not use flakes (yet), as I already have a toplevel nixpkgs
#    (zwizwa), and nixpkgs package descriptions that can be used with
#    overlays, so everything is already pinned
#
# 2. Provide a package description for this repository, for eventual
#    release.  For now however only the builddeps are used.  This is
#    structured as a nixpkgs package (nix/pkgs/dev) and a wrapper that
#    binds it to nixpkgs and installs some overlays from nix/pkgs/* if
#    needed.
#
# 3. Use cached-nix-shell to make incremental builds faster
#
# 4. FIXME: uc_tools is not yet part of dev.nix

NPROC=$(nproc)
cd $(dirname $0)

# NIX_SHELL=nix-shell
NIX_SHELL=cached-nix-shell

$NIX_SHELL nix/dev.nix --run "make -j${NPROC}"

