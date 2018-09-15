#!/bin/bash

# See # https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/

# TODO:
# - nix?

need_target() {
    if ! rustup show |grep $1; then
        rustup target add $1
    fi
}

# Cortex M3
need_target thumbv7m-none-eabi

# Cortex M4
# need_target thumbv7em-none-eabihf
