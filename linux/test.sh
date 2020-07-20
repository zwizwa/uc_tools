#!/bin/bash
set -e
OUT="$1"
cd $(dirname $0)
[ -z "$OUT" ] && OUT=test.log
echo "writing to $OUT" >&2
run() {
    echo "==== $1"
    ./$1 2>&1
}

(
run test_buf.dynamic.host.elf
run test_feynman.dynamic.host.elf
run test_misc.dynamic.host.elf
run test_ns_queue.dynamic.host.elf
run test_pdm.dynamic.host.elf
run test_heap.dynamic.host.elf
run test_csp.dynamic.host.elf
run test_sm_csp.dynamic.host.elf 
) >$OUT
set -x
git diff test.log
