#!/bin/sh
set -x
HERE=$(dirname $0)
exec socat PTY,rawer,link=/tmp/test EXEC:$HERE/test_telnet.dynamic.host.elf

# picocom /tmp/test


