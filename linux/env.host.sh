# Since we support multiple platform build configurations anyway, it
# is easy to drop in this file and also support building a lib.host.a
# to test platform independent code on linux.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..
[ -z "$GCC" ] && GCC=gcc

# Only enable -Werror for development builds!
# CFLAGS_ERROR=-Werror
[ -z "$CFLAGS_ERROR" ] && CFLAGS_ERROR=-Wno-error

# The path of least resistance is to just use gnu99 on linux.
# -std=c99



CFLAGS=" \
-std=gnu99 \
-I$UC_TOOLS/linux \
-I$UC_TOOLS/ \
-I$LIBOPENCM3/include \
-fno-common \
-ffunction-sections \
-fdata-sections \
-MD \
-Wall \
-Wno-format-security \
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
