#!/bin/sh
[ -z "$2" ] && echo "usage: $0 <target.dsk> <embed.ipxe>" && exit 1
cd $(dirname "$0")
set -x
cat <<EOF | cached-nix-shell shell.nix --exec ipxe-fhs
make -C src bin/$1 NO_WERROR=1 EMBED=$(readlink -f "$2") 
EOF
