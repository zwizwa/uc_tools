# These are the variables shared between all architecutres.  See
# env.$ARCH.sh for architecutre specific variables.

# ---- Configuration

# If this is not defined, assume we're in one of the build
# directories, one down from top dir.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..

# Tool Prefix
[ ! -z "$GCC_ARM_NONE_EABI_PREFIX" ] && TPF="$GCC_ARM_NONE_EABI_PREFIX"

# By default, assume the tools are linked relative to uc_tools directory.
# [ -z "$TPF" ] && TPF=$UC_TOOLS/../gcc-arm-none-eabi-4_8-2014q2/bin/arm-none-eabi-
[ -z "$TPF" ] && TPF=$UC_TOOLS/../gcc-arm-none-eabi/bin/arm-none-eabi-

[ -z "$LIBOPENCM3" ] && LIBOPENCM3=$UC_TOOLS/../libopencm3

# ---- Implementation

GCC=${TPF}gcc
OBJDUMP=${TPF}objdump
OBJCOPY=${TPF}objcopy
READELF=${TPF}readelf
ELFTYPE=elf32-littlearm
BINARCH=arm

MFLAGS_COMMON=""

CPPFLAGS_COMMON="\
-I$UC_TOOLS/gdb \
-I$UC_TOOLS/ \
-I$LIBOPENCM3/include \
"

# CFLAGS_ERROR=-Werror
# CFLAGS_ERROR=

# Note: to see which gnu extensions are used, set -std=c99

# -std=gnu99

if [ -z "$CFLAGS_OPTI" ]; then
    CFLAGS_OPTI=-Os
#else
#    echo "override CFLAGS_OPTI=$CFLAGS_OPTI"
fi

CFLAGS_COMMON=" \
-std=c99 \
-fno-common \
-ffunction-sections \
-fdata-sections \
-MD \
-Wall -Wundef \
-Wno-format \
-Wno-attributes \
-Wno-multichar \
-g \
$CFLAGS_OPTI \
$CFLAGS_ERROR \
"

LDFLAGS_COMMON=" \
-g \
--static \
-nostartfiles \
-L$LIBOPENCM3/lib \
-Wl,--gc-sections \
"

LDLIBS_COMMON="\
-Wl,--start-group \
-lc \
-lgcc \
-Wl,--end-group \
"
