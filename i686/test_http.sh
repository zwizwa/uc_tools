#!/bin/sh
# HTTP and websocket console through the lwip plugin (i686/lwip_plugin.c).
#   test_http.sh <ip> [<local ip>]
# - GET / serves www/index.html, unknown paths 404
# - 20 GETs in a row, each on a new connection
# - websocket at /ws: the RFC 6455 example key gives the example
#   accept, a masked binary frame with a line runs it in forth and the
#   output comes back as frames
# - a 9000 byte websocket frame only drops that connection
# See test_tcp_console.sh for <local ip>.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip> [<local ip>]"; exit 1; }
BIND=${2:+,bind=$2}
IFACE=${2:+--interface $2}

push_kernel
push_plugin "$I686/lwip_plugin.bin"

out=$(curl -s -m 5 $IFACE -i http://$T/)
check "GET /" sh -c 'echo "$0" | grep -a -q "^HTTP/1.0 200" && echo "$0" | grep -a -q "<title>"' "$out"
out=$(curl -s -m 5 $IFACE -i http://$T/nope)
check "GET 404" sh -c 'echo "$0" | grep -a -q "^HTTP/1.0 404"' "$out"
n=$(for i in $(seq 20); do curl -s -m 2 $IFACE http://$T/; done | grep -a -c "<title>")
check "20 GETs" [ "$n" = 20 ]

upgrade() {
    printf "GET /ws HTTP/1.1\r\nHost: %s\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" $T
    printf "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
}
# binary frame, mask key 0 so the payload is plain
out=$({ upgrade; sleep 1; printf "\202\205\0\0\0\0hello"; sleep 2; } |
    timeout 6 socat - TCP:$T:80$BIND 2>/dev/null | tr -d "\r")
check "ws accept" sh -c 'echo "$0" | grep -a -q "^Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=$"' "$out"
check "ws line" sh -c 'echo "$0" | grep -a -q "hello!$"' "$out"

# 127: 64 bit length 9000 > WS_LEN_MAX
{ upgrade; sleep 1; printf "\202\377\0\0\0\0\0\0\043\050"; sleep 2; } |
    timeout 6 socat - TCP:$T:80$BIND >/dev/null 2>&1
check "alive after big frame" sh -c 'echo "$0" | grep -a -q "hello!"' "$(CON_DELAY=2 con hello)"
finish
