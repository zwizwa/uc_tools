#!/bin/bash
if [ "$1" != "start" ]; then
    echo "FIXME: Non-daemon mode is not implemented for $0.  Use \"$0 start\" instead." && exit 0
fi

TEST_LUA=test_la_lua51.lua

# FIXME: Figure out how to set breakpoints in dlopen libs.

exec gdb 2>&1 \
     --quiet \
     --eval-command="set confirm off" \
     --eval-command="set set stop-on-solib-events 1" \
     --eval-command="file /usr/bin/lua5.1" \
     --eval-command="break cmd_send" \
     --eval-command="run $TEST_LUA" \
     --eval-command="bt"


