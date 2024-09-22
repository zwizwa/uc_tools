#!/bin/sh
mkdir -p ~/.nix-develop
PROFILE=~/.nix-develop/esp32-idf
nix develop 'github:mirrexagon/nixpkgs-esp-dev#esp32-idf' --profile $PROFILE
