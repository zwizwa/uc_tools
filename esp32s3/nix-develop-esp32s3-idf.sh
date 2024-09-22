#!/bin/sh
mkdir -p ~/.nix-develop
PROFILE=~/.nix-develop/esp32s3-idf
nix develop 'github:mirrexagon/nixpkgs-esp-dev#esp32s3-idf' --profile $PROFILE --verbose --print-build-logs
