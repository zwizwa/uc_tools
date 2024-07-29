# If this is not defined, assume it is current working directory.
[ -z "$UC_TOOLS" ] && UC_TOOLS=..

. $UC_TOOLS/stm32f103/env.common.sh

# See libopencm3/lib/stm32/f4/Makefile for compiler flags.

MFLAGS=" \
-mcpu=cortex-m4 \
-mthumb \
-mfloat-abi=hard -mfpu=fpv4-sp-d16 \
-mfix-cortex-m3-ldrd \
" 

CPPFLAGS="$CPPFLAGS_COMMON"
CFLAGS="$CFLAGS_COMMON $CPPFLAGS $MFLAGS -DSTM32F4"
LDFLAGS="$LDFLAGS_COMMON $MFLAGS"
LDLIBS="$LDLIBS_COMMON -lopencm3_stm32f4"

ELFS="
bl_f4.core.f407.elf
"

O_SYSTEM=registers_stm32f407.f407.o
