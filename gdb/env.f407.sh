. ./env.common.sh

MFLAGS=" \
-mthumb \
-mcpu=cortex-m4 \
-mhard-float \
-mfix-cortex-m3-ldrd \
" 

CPPFLAGS="$CPPFLAGS_COMMON"
CFLAGS="$CFLAGS_COMMON $CPPFLAGS $MFLAGS -DSTM32F4"
LDFLAGS="$LDFLAGS_COMMON $MFLAGS"
LDLIBS="$LDLIBS_COMMON -lopencm3_stm32f4"
ELF="$ELF_COMMON"

ELF="
bl_f4.core.f407.elf
"

O_SYSTEM=registers_stm32f407.f407.o
