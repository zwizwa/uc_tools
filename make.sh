#!/bin/sh

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

