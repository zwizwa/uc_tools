#!/bin/sh

# Seems simpler to use the full package.
$(dirname "$0")/../nix-develop-esp-idf.sh
exit 0

mkdir -p ~/.nix-develop
PROFILE=~/.nix-develop/esp32c3-idf
nix develop 'github:mirrexagon/nixpkgs-esp-dev#esp32c3-idf' --profile $PROFILE --verbose --print-build-logs
