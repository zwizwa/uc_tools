#!/bin/sh
[ -z "$2" ] && echo "usage: $0 <target.dsk> <embed.ipxe>" && exit 1
cd $(dirname "$0")
set -x
# It's a real pain to pass arguments across two levels of wrapping
# (nix shell and fhs bwrap script) so just run the shell and pipe
# commmands into it.
cat <<EOF | cached-nix-shell ipxe-fhs.nix --exec ipxe-fhs
make -C src bin/$1 NO_WERROR=1 EMBED=$(readlink -f "$2") 
EOF
