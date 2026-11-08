#!/bin/sh
# Console over TCP port 23 through the lwip plugin (i686/lwip_plugin.c).
#   test_tcp_console.sh <ip> [<local ip>]
# - lines typed over TCP run in forth, output comes back with \r\n
# - output caused elsewhere (UDP console) shows up too
# - 200 lines in one burst all get answered
# - a second connection while the first is open, and reconnects
# - ctrl-d on an empty line closes the connection
# lwIP has no gateway: the client address must be on its /24.  The
# qemu rig is routed, not bridged, so pass psamathe's br9 address.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip> [<local ip>]"; exit 1; }
BIND=${2:+,bind=$2}

# tcp <seconds> <line>...: one connection, lines 1s apart
tcp() {
    s=$1; shift
    { sleep 1; for l in "$@"; do printf "%s\r\n" "$l"; sleep 1; done; sleep $s; } |
        timeout $((s + 3 + $#)) socat - TCP:$T:23$BIND 2>/dev/null | tr -d "\r"
}

push_kernel
push_plugin "$I686/lwip_plugin.bin"

out=$(tcp 1 hello)
check "line over tcp" sh -c 'echo "$0" | grep -a -q "^hello!$"' "$out"

tcp 4 >$HERE/tcp1.log &
sleep 2
CON_DELAY=1 con hello >/dev/null
wait
check "udp console output on tcp" grep -a -q "^hello!$" $HERE/tcp1.log

n=$({ sleep 1; for i in $(seq 200); do printf "hello\r\n"; done; sleep 5; } |
    timeout 10 socat - TCP:$T:23$BIND 2>/dev/null | grep -a -c "^hello!")
echo "burst $n"
check "200 line burst" [ "$n" = 200 ]

tcp 4 >$HERE/tcp1.log &
sleep 1
out=$(tcp 1 hello)
wait
check "second connection" sh -c 'echo "$0" | grep -a -q "^hello!$"' "$out"
for i in 1 2 3 4 5 6; do out=$(tcp 0 hello); done
check "reconnects" sh -c 'echo "$0" | grep -a -q "^hello!$"' "$out"

# ctrl-d closes on an empty line only: socat exits on the close
# before timeout kills it (124)
{ sleep 1; printf "\004"; sleep 5; } | timeout 4 socat - TCP:$T:23$BIND >/dev/null 2>&1
check "ctrl-d closes" [ $? != 124 ]
out=$({ sleep 1; printf "hel\004lo\r\n"; sleep 2; } |
    timeout 5 socat - TCP:$T:23$BIND 2>/dev/null | tr -d "\r")
check "ctrl-d in a line is ignored" sh -c 'echo "$0" | grep -a -q "^hello!$"' "$out"
rm -f $HERE/tcp1.log
finish
