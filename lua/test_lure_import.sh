#!/bin/sh
# This is simpler to do in shell.
cd $(dirname $0)
rm -f lure/bundle.lua
BUNDLE=$(mktemp)
echo "local bundle = {" >$BUNDLE
find lure -name '*.lua' | sort | while read line; do
      bn=$(basename $line .lua)
      echo "  $bn = require('lure.$bn')," >>$BUNDLE
done
echo "}">>$BUNDLE
echo "bundle.test(bundle)"
mv $BUNDLE lure/bundle.lua
echo "require ('lure.log') ; log_desc(require('lure.bundle'))" | lua

