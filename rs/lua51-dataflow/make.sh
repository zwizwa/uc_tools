#!/bin/sh

# Currently 2025-04-17 it seems that etc_net dev.nix is set up with
# cargo in the path, so just run it like this for now.

set -xe

cd $(dirname "$0")
cargo build
lua test.lua


