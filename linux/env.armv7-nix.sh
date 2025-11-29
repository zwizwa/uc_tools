# Note 2025-11-08: Trying something else.  This script is like
# env.host.sh but it uses an env.sh generated from the nix build
# environment.  This needs to be provided explicitly.
if [ -z "$ARMV7_NIX_DEV" ]; then
    echo "error: need ARMV7_NIX_DEV" >&2
    exit 1
fi
# echo "using ARMV7_NIX_DEV=$ARMV7_NIX_DEV" >&2
. $ARMV7_NIX_DEV/env.sh
# echo "using GCC=$GCC" >&2

if [ -z "$GCC" ]; then
    echo "GCC not set in $ARMV7_NIX_DEV/env.sh"
    exit 1
fi


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
-Wno-stringop-overflow \
-Wno-zero-length-bounds \
-Wno-array-bounds \
-Wunused-result \
-g \
-O3 \
-fPIC \
$CFLAGS_ERROR \
"
LDFLAGS=" \
-lm \
"
