#!/bin/bash
set -e
cd $(dirname $0)
run() {
    echo "==== $1"
    ./$1 2>&1
}

run test_buf.dynamic.host.elf
run test_feynman.dynamic.host.elf
run test_misc.dynamic.host.elf
run test_ns_queue.dynamic.host.elf
run test_pdm.dynamic.host.elf
