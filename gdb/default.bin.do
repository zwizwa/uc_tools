. ./env.common.sh
redo-ifchange $2.elf
$OBJCOPY -O binary $2.elf $3
