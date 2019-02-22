# $1 target
# $2 basename
# $3 temp

. ./env.sh

# Filename encodes which linker script to use.
# E.g. for foo.x8.elf, the script is x8.ld
# Variables below are annotated with the foo example:

# foo.x8
APP_TYPED=$(basename $1 .elf)
# x8
TYPE="${APP_TYPED##*.}"
# foo
APP=$(basename $APP_TYPED .$TYPE)
# foo.ld
LD=$TYPE.ld
# foo.x8.map
MAP=$APP_TYPED.map
# foo.o
O=$APP.o

redo-ifchange $O lib.a $LD $UC_TOOLS/registers_stm32f103.o
$GCC $LDFLAGS -T$LD -Wl,-Map=$MAP -o $3 $O $UC_TOOLS/registers_stm32f103.o lib.a $LDLIBS

