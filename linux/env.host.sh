# Since we support multiple platform build configurations anyway, it
# is easy to drop in this file and also support building a lib.host.a
# to test platform independent code on linux.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..
[ -z "$GCC" ] && GCC=gcc


# Note that this used to default to -Werror disabled, but it has
# caused a lot of issues in the past so enable it by default.  It can
# still be disabled if necessary (e.g. in the Nix package build there
# are some harmless warnings).

[ -z "$CFLAGS_ERROR" ] && CFLAGS_ERROR=-Werror


# Keep optimization enabled by default.  Also, nix complains:
# warning _FORTIFY_SOURCE requires compiling with optimization (-O)

# Note that the path of least resistance is to just use gnu99 on linux.


CFLAGS="$CFLAGS \
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
-O3 \
-fPIC \
$CFLAGS_ERROR \
"
LDFLAGS=" \
-lm \
"
