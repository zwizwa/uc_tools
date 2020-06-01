#!/bin/bash
cd $(dirname $0)
. ./env.sh

cat <<EOF >cscope.files
$(ls -1 $ZEPHYR_BASE/include/bluetooth/*.[ch])
$(ls -1 $(readlink -f .)/src/*.[ch])
EOF
cscope -kbq

