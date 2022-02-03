#!/bin/sh
cd $(dirname $0)

if [ ! -z "$1" ]; then
    exec lua -e "require('lure.$1').run()"
fi

# It doesn't seem possible to install
# arbitrary assets, so bundle them up in a lua module.

(
echo "# -*- scheme -*-"
echo "return {"
for file in *.sm *.scm; do
    echo "['$file'] = [["
    cat $file
    echo "]],"
done
echo "}"
) >lure/asset_scm.lua

# Generate the lure/meta.lua file
META=lure/meta.lua
echo "return { modules = {" >$META
find lure -name '*.lua' | sort | while read line; do
      bn=$(basename $line .lua)
      echo "  $bn = true," >>$META
done
echo "}}">>$META

LOG=$(readlink -f ./test_lure.log)
lua -e "require ('lure.test').run()" >$LOG 2>&1
ERR=$?
if [ "$ERR" != 0 ]; then
    echo "ERR: $ERR"
    echo "see $LOG"
    tail -n 30 $LOG
    exit $ERR
fi
echo "test OK"

set -e

# Generate the rockspec
VER=0.1
ROCKSPEC=$(readlink -f ./rockspec/lure-$VER-1.rockspec)
echo "require('lure.test').gen_rockspec('$VER')" | lua | unix2dos > $ROCKSPEC


# Diff the rockspec
(cd $(dirname $ROCKSPEC)
 git diff $(basename $ROCKSPEC))

# If we get this far, update the git repo.
# (cd ~/git/lure-lua ; ./update.sh)





