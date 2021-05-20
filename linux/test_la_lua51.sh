#!/bin/bash
if [ "$1" != "start" ]; then
    echo "FIXME: Non-daemon mode is not implemented for $0.  Use \"$0 start\" instead." && exit 0
fi

SO=la_lua51.so
HERE=$(dirname $0)
TEST_LUA=test_la_lua51.lua
exec gdb 2>&1 \
     --quiet \
     --eval-command="file $HERE/$SO" \
     --eval-command="run /usr/bin/lua-5.1 $TEST_LUA" \
     --eval-command="bt"
