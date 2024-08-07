#!/bin/sh

[ -z "$2" ] && echo "usage: $0 <host> <port>" && exit 1

GDBDIR=$(readlink -f $(dirname $0))
HOST="$1"
PORT="$2"

INITSCRIPT=$GDBDIR/.gdb_$(hostname)_to_${HOST}_${PORT}

TARGET="$HOST:$PORT"
if [ $HOST = "tty" ]; then
    TARGET="/dev/$PORT"
fi

TEST=$GDBDIR/test.gdb

# To resolve types, GDB needs an ELF loaded.
ELF=../stm32f103/bl_c8t6_a12b5_boot1.core.f103.elf



cat <<EOF >$INITSCRIPT
$(cat $GDBDIR/lib.gdb)
define connect
  target remote $TARGET
end
define test
  shell $GDBDIR/test.sh $TEST
  source $TEST
end
file $ELF
connect
EOF

echo "Using OpenOCD $HOST:$PORT" >&2
[ -z "$GDB" ] && GDB=arm-eabi-gdb

exec $GDB -quiet -x $INITSCRIPT




