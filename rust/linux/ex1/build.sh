#!/bin/bash
# Minimal example without panic code, linking rust static lib into a C program.
# Some notes:
# - optimization is necessary to get rid of the panic handling code
# - link time gc is necessary to not bloat the final binary


set -x
rm -f libadd1.a test
rustc --crate-type=staticlib -C panic=abort -O add1.rs
gcc -o test test.c -L . -ladd1 -Wl,--gc-sections
# strip a.out
ls -l libadd1.a test
./test


