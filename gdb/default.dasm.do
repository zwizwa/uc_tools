# FIXME: this works for .o but is different for .elf

ARCH="${2##*.}"
BN=$(basename $2 .$ARCH)
echo "ARCH=$ARCH BN=$BN" >&2
. ./env.$ARCH.sh
redo-ifchange $2.o
echo "OBJDUMP=$OBJDUMP" >&2
$OBJDUMP -d $2.o >$3
