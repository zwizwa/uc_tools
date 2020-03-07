# Since we support multiple platform build configurations anyway, it
# is easy to drop in this file and also support building a lib.host.a
# to test platform independent code on linux.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..
[ -z "$GCC" ] && GCC=gcc
CFLAGS=" \
-I$UC_TOOLS/linux \
-I$UC_TOOLS/ \
-I$LIBOPENCM3/include \
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
