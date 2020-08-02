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
run test_csp.sh
run test_actor.sh
run test_dataflow.sh
) >$OUT
set -x
git diff test.log
