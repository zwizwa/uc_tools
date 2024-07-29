# If this is not defined, assume it is current working directory.
[ -z "$UC_TOOLS" ] && UC_TOOLS=..

. $UC_TOOLS/stm32f103/env.common.sh


CPPFLAGS="$CPPFLAGS_COMMON"
CFLAGS="$CFLAGS_COMMON $CPPFLAGS $MFLAGS -DSTM32F1"
LDFLAGS="$LDFLAGS_COMMON $MFLAGS"
LDLIBS="$LDLIBS_COMMON -lopencm3_stm32f1"
