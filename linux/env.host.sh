# Since we support multiple platform build configurations anyway, it
# is easy to drop in this file and also support building a lib.host.a
# to test platform independent code on linux.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..
[ -z "$GCC" ] && GCC=gcc

# CFLAGS_ERROR=-Werror
CFLAGS_ERROR=

CFLAGS=" \
-I$UC_TOOLS/linux \
-I$UC_TOOLS/ \
-I$LIBOPENCM3/include \
-std=gnu99 \
-fno-common \
-ffunction-sections \
-fdata-sections \
-MD \
-Wall \
-Wno-format \
-Wno-attributes \
-Wno-multichar \
-Wunused-result \
-g \
-O0 \
-fPIC \
$CFLAGS_ERROR \
"
LDFLAGS=" \
-lm \
"
