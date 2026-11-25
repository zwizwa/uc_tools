#!/usr/bin/env bash
cd $(dirname $0)
[ -z "$ELF" ] && ELF=$(readlink -f ./packet_bridge_main.dynamic.host.elf)

# Test PASS/FAIL is indicated by return value. 0->PASS, other->FAIL

test_hex() {

echo "ELF=$ELF"
cat <<EOF | $ELF HEX -:4 | hexdump -C -v
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
EOF

}

test_usb() {

set -x

# FIXME: This has been postponed in favor of a dedicated program for axo.
# FIXME: This has to run on a physical host.
ABS_ELF="/i/$HOSTNAME/$ELF"
echo ABS_ELF=$ABS_ELF
(ssh 10.1.3.12 $ABS_ELF - USB) 2>&1

}

# TCP ports.  The listener side is an echo, EXEC:4:cat.

NB_FAIL=0
PORT=${PORT:-51500}
TMP=$(mktemp -d)
trap 'rm -rf $TMP' EXIT

check() { # <name> <rc>
    if [ "$2" = 0 ]; then echo "PASS $1"; else echo "FAIL $1"; NB_FAIL=$((NB_FAIL+1)); fi
}

# Expects {packet,4} data on stdin of the client to come back.  The
# client exits on eof, so keep stdin open until all data is back.
echo_client() { # <spec> <in> <out>
    mkfifo $TMP/fifo
    timeout 20 $ELF -:4 "$1" <$TMP/fifo >$3 2>$TMP/client.err &
    local pid=$!
    exec 3>$TMP/fifo
    cat $2 >&3
    local size=$(stat -c %s $2)
    for i in $(seq 100); do
        [ $(stat -c %s $3) -ge $size ] && break
        kill -0 $pid 2>/dev/null || break
        sleep 0.1
    done
    exec 3>&-
    wait $pid
    rm $TMP/fifo
    cmp $2 $3
}

# The port is open in /proc/net/tcp* (state 0A = LISTEN).
wait_listen() { # <port>
    local hex=$(printf ':%04X' $1)
    for i in $(seq 50); do
        cat /proc/net/tcp /proc/net/tcp6 2>/dev/null |
            awk -v p=$hex '$4 == "0A" && substr($2, length($2)-4) == p {f=1} END {exit !f}' && return 0
        sleep 0.1
    done
    return 1
}

test_tcp_echo() { # <listen type> <framing> <connect spec prefix>
    PORT=$((PORT+1))
    timeout 30 $ELF $1:$2:$PORT EXEC:4:cat &
    local pid=$!
    wait_listen $PORT
    check "$1:$2 listen" $?
    echo_client "$3:$PORT" $TMP/data $TMP/out
    check "$1:$2 echo" $?
    wait $pid
    check "$1:$2 exit 0 on close" $?
}

# The listener is closed after accept, and can be restarted on the
# same port right after the connection is gone.
test_tcp_restart() { # <listen type> <connect spec prefix>
    PORT=$((PORT+1))
    for n in 1 2 3; do
        timeout 30 $ELF $1:4:$PORT EXEC:4:cat &
        local pid=$!
        wait_listen $PORT
        echo_client "$2:$PORT" $TMP/data $TMP/out
        check "$1 restart $n" $?
        wait $pid
    done
}

# One connection only, and the second port is opened on connect.
test_tcp_order() {
    PORT=$((PORT+1))
    rm -f $TMP/opened
    printf '#!/bin/sh\ntouch %s/opened\nexec cat\n' $TMP >$TMP/exec.sh
    chmod +x $TMP/exec.sh
    timeout 30 $ELF TCP6-LISTEN:4:$PORT EXEC:4:$TMP/exec.sh &
    local pid=$!
    wait_listen $PORT
    sleep 0.2
    [ ! -e $TMP/opened ]
    check "second port not open before connect" $?
    sleep 2 | timeout 10 $ELF -:4 "TCP6:4:[::1]:$PORT" &
    local client=$!
    for i in $(seq 20); do [ -e $TMP/opened ] && break; sleep 0.1; done
    [ -e $TMP/opened ]
    check "second port open after connect" $?
    ! timeout 5 $ELF -:4 "TCP6:4:[::1]:$PORT" </dev/null 2>/dev/null
    check "second connection refused" $?
    wait $client $pid
}

# TCP6-LISTEN does not take IPv4.
test_tcp6_only() {
    PORT=$((PORT+1))
    timeout 30 $ELF TCP6-LISTEN:4:$PORT EXEC:4:cat &
    local pid=$!
    wait_listen $PORT
    ! timeout 5 $ELF -:4 TCP:4:127.0.0.1:$PORT </dev/null 2>/dev/null
    check "TCP6-LISTEN refuses IPv4" $?
    kill $pid
    wait $pid 2>/dev/null
}

test_tcp6_bad_spec() {
    local spec
    for spec in 'TCP6:4:::1:51499' 'TCP6:4:[foo]:51499' 'TCP6:4:[fe80::1%nope]:51499'; do
        ! timeout 5 $ELF -:4 "$spec" </dev/null 2>/dev/null
        check "rejects $spec" $?
    done
}

test_tcp() {
    ulimit -c 0  # ASSERT dumps core
    # Random packets, for slip this includes the bytes that are escaped.
    for i in $(seq 200); do
        printf '\000\000\017\240'
        head -c 4000 /dev/urandom
    done >$TMP/data
    # TCP:slip is not implemented
    test_tcp_echo TCP-LISTEN  4    TCP:4:127.0.0.1
    test_tcp_echo TCP6-LISTEN 4    TCP6:4:[::1]
    test_tcp_echo TCP6-LISTEN slip TCP6:slip:[::1]
    test_tcp_restart TCP-LISTEN  TCP:4:127.0.0.1
    test_tcp_restart TCP6-LISTEN TCP6:4:[::1]
    test_tcp_order
    test_tcp6_only
    test_tcp6_bad_spec
}

test_hex
test_tcp

[ $NB_FAIL = 0 ]
exit $?

