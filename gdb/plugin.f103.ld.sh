#!/bin/sh

# Dervied from flash_plugin.link.sh
# FIXME: Remove old .do files once makefile gen works properly.

[ -z "$PARENT_ELF" ] && echo "need PARENT_ELF" && exit 1
[ -z "$BASE_LD" ] && echo "need BASE_LD" && exit 1
[ -z "$MEM_TOP" ] && echo "need MEM_TOP" && exit 1
# echo "PARENT_ELF=$PARENT_ELF" >&2

cat <<EOF
$($MEM_TOP $PARENT_ELF)
INCLUDE ${BASE_LD}
SECTIONS {
    .flash_pad : {
        . = ALIGN(1024);
        _eplugin = . ;     
     } >rom
}

EOF
