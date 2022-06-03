# See comments in env.host.sh
# This is dame, but optimization disabled.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..
[ -z "$GCC" ] && GCC=gcc

# Only enable -Werror for development builds!
CFLAGS_ERROR=-Werror
[ -z "$CFLAGS_ERROR" ] && CFLAGS_ERROR=-Wno-error

CFLAGS="$CFLAGS \
-std=gnu99 \
-I$UC_TOOLS/linux \
-I$UC_TOOLS/ \
-I$LIBOPENCM3/include \
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
