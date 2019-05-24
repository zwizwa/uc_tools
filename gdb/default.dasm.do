. ./env.common.sh
redo-ifchange $2.o
$OBJDUMP -d $2.o >$3
