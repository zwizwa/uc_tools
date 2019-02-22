# $1 target
# $2 basename
# $3 temp

. ./env.sh
redo-ifchange lib.a $2.o core.ld stm32f1.ld ../registers_stm32f103.o 
$GCC $LDFLAGS -Tcore.ld -Wl,-Map=$2.map -o $3 $2.o ../registers_stm32f103.o lib.a $LDLIBS
