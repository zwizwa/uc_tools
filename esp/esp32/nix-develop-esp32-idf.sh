#!/bin/sh

# Seems simpler to use the full package.
$(dirname "$0")/../nix-develop-esp-idf.sh
exit 0

mkdir -p ~/.nix-develop
PROFILE=~/.nix-develop/esp32-idf

# empty flake registry is to allow offline work
nix develop \
    --flake-registry "" \
    'github:mirrexagon/nixpkgs-esp-dev#esp32-idf' --profile $PROFILE
