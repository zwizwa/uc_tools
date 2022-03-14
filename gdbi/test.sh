#!/bin/sh

# The 'test' command invokes this script, then sources test.gdb
# The test.gdb is compiled from test.scm

# I have no idea where this will end up, but the idea is to have a "no
# dependency" setup: attach microcontroller to gdb via stdlink, and
# start messing with it without relying on any code on the controller.
# Using scheme as source languages because GDB script can't do a whole
# lot.

[ -z "$1" ] && echo "usage: $0 <file>" && exit 1
OUT=$(readlink -f "$1")
IN=$(readlink -f $(dirname $0)/test.scm)
# echo generating $OUT >&2
cd $(dirname $0)/../lua
CMD="require('lure')['gdb']('$IN')"
# set -x
cat <<EOF >$OUT
$(lua -e "$CMD")
print \$rv
EOF

# cat $OUT >&2



