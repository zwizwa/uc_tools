#!/bin/sh

# FIXME: This depends on rdm-bridge-dev

old() {

RDM_BRIDGE_DEV=~/.nix-c8/rdm-bridge-dev
ENV=$RDM_BRIDGE_DEV/env

if [ -e $ENV ]; then
	. $ENV
	export PATH="$RDM_BRIDGE_DEV_PATH:$PATH"
        export RDM_BRIDGE_DEV
fi

# Fish it from path
LUA=$(which lua)

}



new() {
LUA=~/.result/lua/bin/lua
}

new
# echo "LUA=$LUA" >&2


# echo "LUA=$LUA" >&2

# Add absolute links to this directory, in case we invoke this script
# from another directory.  This needs to be done for 3 search paths:
# .lua scripst, lua binary .so modules, and shell scripts invoked by
# test code.

HERE=$(readlink -f $(dirname $0))
export LUA_PATH="${HERE}/?.lua;${HERE}/lib/?.lua${LUA_PATH:+';'}${LUA_PATH}"
export LUA_CPATH="${HERE}/?.so${LUA_CPATH:+';'}${LUA_CPATH}"
export PATH="${HERE}:$PATH"

exec $LUA "$@"
