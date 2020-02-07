# These are the variables shared between all architecutres.  See
# env.$ARCH.sh for architecutre specific variables.

# ---- Configuration

TOOL_DIR=../../gcc-arm-none-eabi-4_8-2014q2
TPF=$TOOL_DIR/bin/arm-none-eabi
LIBOPENCM3=../../libopencm3
UC_TOOLS=..
VERSION=$(git rev-parse HEAD)

# ---- Implementation

GCC=${TPF}-gcc
OBJDUMP=${TPF}-objdump
OBJCOPY=${TPF}-objcopy
READELF=${TPF}-readelf

MFLAGS_COMMON=""

CPPFLAGS_COMMON="\
-I. \
-I.. \
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
