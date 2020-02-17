# These are the variables shared between all architecutres.  See
# env.$ARCH.sh for architecutre specific variables.

# ---- Configuration

# If this is not defined, assume it is current working directory.
[ -z "$UC_TOOLS_GDB_DIR" ] && UC_TOOLS_GDB_DIR=.

TOOL_DIR=$UC_TOOLS_GDB_DIR/../../gcc-arm-none-eabi-4_8-2014q2
TPF=$TOOL_DIR/bin/arm-none-eabi
LIBOPENCM3=$UC_TOOLS_GDB_DIR/../../libopencm3
UC_TOOLS=$UC_TOOLS_GDB_DIR/..
VERSION=$(cd $UC_TOOLS ; git rev-parse HEAD)

# ---- Implementation

GCC=${TPF}-gcc
OBJDUMP=${TPF}-objdump
OBJCOPY=${TPF}-objcopy
READELF=${TPF}-readelf

MFLAGS_COMMON=""

CPPFLAGS_COMMON="\
-I$UC_TOOLS_GDB_DIR. \
-I$UC_TOOLS_GDB_DIR/.. \
-I$UC_TOOLS/include \
-I$LIBOPENCM3/include \
"

CFLAGS_COMMON=" \
-std=c99 \
-fno-common \
-ffunction-sections \
-fdata-sections \
-MD \
-Wall \
-Werror \
-Wno-format \
-Wno-attributes \
-Wno-multichar \
-g \
-Os \
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
