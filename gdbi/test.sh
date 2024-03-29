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
rm -f $OUT

exit_clean() {
    rm -f $OUT.stdout $OUT.stderr    
    cat $OUT >&2
    exit $1
}

# Can't propagate error condition to gdb, so handle that here and
# display the error while returning an empty file for GDB.
if lua -e "$CMD" >$OUT.stdout 2>$OUT.stderr; then
cat <<EOF >$OUT
# -*- gdb-script -*-
$(cat $OUT.stdout)
print \$rv
EOF
exit_clean 0
else
cat $OUT.stderr >&2
cat <<EOF >$OUT
EOF
exit_clean 1
fi
