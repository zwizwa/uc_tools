#!/bin/sh
cd $(dirname $0)
cat rvm_debug.lua |grep 'local input'
cat rvm.lua | \
sed -e 's|.*--debug--.*||g' | \
sed -e 's|[ \t]*--.*||g' | \
sed -e 's|^[ \t]*||g' | \
sed -e '/^[ \t]*$/d'
