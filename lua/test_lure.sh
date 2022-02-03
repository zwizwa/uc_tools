#!/bin/sh
cd $(dirname $0)

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
echo "require ('lure.test').run()" | lua
