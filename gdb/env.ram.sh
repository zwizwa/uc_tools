# If this is not defined, assume it is current working directory.
[ -z "$UC_TOOLS" ] && UC_TOOLS=..

. $UC_TOOLS/gdb/env.common.sh

MFLAGS=" \
-mthumb \
-mcpu=cortex-m3 \
-msoft-float \
-mfix-cortex-m3-ldrd \
" 

CPPFLAGS="$CPPFLAGS_COMMON"
CFLAGS="$CFLAGS_COMMON $CPPFLAGS $MFLAGS -DSTM32F1 $CFLAGS"
LDFLAGS="$LDFLAGS_COMMON $MFLAGS"
LDLIBS="$LDLIBS_COMMON -lopencm3_stm32f1"

O_SYSTEM=$UC_TOOLS/gdb/registers_stm32f103.f103.o

