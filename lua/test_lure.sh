#!/bin/sh
cd $(dirname $0)

# Perform some file generation
# 1. Assets
# It doesn't seem possible to install
# arbitrary assets, so bundle them up in a lua module.

( cd lure
echo "# -*- scheme -*-"
echo "return {"
for file in *.sm *.scm; do
    echo "['$file'] = [["
    cat $file
    echo "]],"
done
echo "}"
) >lure/asset_scm.lua

( cd lure
echo "# -*- c -*-"
echo "return {"
for file in *.h; do
    echo "['$file'] = [["
    cat $file
    echo "]],"
done
echo "}"
) >lure/asset_c.lua


# 2. Generate the lure/meta.lua file
META=lure/meta.lua
echo "return { modules = {" >$META
find lure -name '*.lua' | sort | while read line; do
      bn=$(basename $line .lua)
      echo "  '$bn'," >>$META
done
echo "}}">>$META



# If requested run only a single test without redirecting output.
if [ ! -z "$1" ]; then
    exec lua -e "require('lure.$1').run()"
fi

# Proceed with rest, redirecting output, only printing tail on failure.
LOG_OUT=$(readlink -f ./test_lure.out)
LOG_ERR=$(readlink -f ./test_lure.err)
lua -e "require ('lure.test').run()" >$LOG_OUT 2>$LOG_ERR
ERR=$?
if [ "$ERR" != 0 ]; then
    # tail -n 30 $LOG
    # just print the one that has the backtrace
    tail -n 100 $LOG_ERR 
    echo "ERR: $ERR"
    echo "see:"
    echo $LOG_OUT
    echo $LOG_ERR
    exit $ERR
fi
echo "test OK"

# All the rest should succeed
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





