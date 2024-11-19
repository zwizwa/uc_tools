#!/bin/sh
mkdir -p ~/.nix-develop
DEVSHELL=esp-idf-full
PROFILE=~/.nix-develop/${DEVSHELL}
nix develop \
    --flake-registry "" \
    "github:mirrexagon/nixpkgs-esp-dev#${DEVSHELL}" \
    --profile $PROFILE \
    --verbose \
    --print-build-logs

