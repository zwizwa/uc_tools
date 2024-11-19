#!/bin/sh

# Seems simpler to use the full package.
$(dirname "$0")/../nix-develop-esp-idf.sh
exit 0

# Only esp32s3
mkdir -p ~/.nix-develop
PROFILE=~/.nix-develop/esp32s3-idf
nix develop \
    --flake-registry "" \
    'github:mirrexagon/nixpkgs-esp-dev#esp32s3-idf' \
    --profile $PROFILE \
    --verbose \
    --print-build-logs
