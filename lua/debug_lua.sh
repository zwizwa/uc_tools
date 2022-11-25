#!/bin/sh
# Yeah fuck that this is the simplest way to do these things.
cd /i/tom/uc_tools/lua
_sed_uc_tools() {
    sed -e s/.*uc_tools/\\/i\\/tom\\/uc_tools/g
}
_sed_lure() {
    sed -e s/.*lure/\\/i\\/tom\\/uc_tools\\/lure/g
}

# ./lua.sh -e "(require 'lure.seq_scm').run()" 2>&1 | _sed_uc_tools
./test_lure.sh 2>&1 | _sed_lure
echo
