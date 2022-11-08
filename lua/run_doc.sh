#!/bin/sh
set -e
[ -z "$1" ] && echo "usage: $0 <file.md>" && exit 1
[ ! -f "$1" ] && echo "file $1 not found" && exit 1
FILE=$(readlink -f "$1")

mv ${FILE} ${FILE}.orig

cd $(dirname $0)
CMD="require('run_doc').run_doc('${FILE}.orig')"
echo "CMD=$CMD"
./lua.sh -e "$CMD" >${FILE}.tmp
mv ${FILE}.tmp ${FILE}
