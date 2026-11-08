# Shared by the test_i686 scripts, sourced.  Run as root on the host
# that reaches the target on-link (psamathe for br54): the kernel has
# no gateway, so its UDP replies only go to its own segment.
#
# Environment:
#   I686    dir with kernel.3if kernel.bin sqlite_plugin.bin
#           test_timer_plugin.bin
#   TETHER  tether_udp command, may include a loader, e.g. when it is
#           built against another glibc, with that glibc in glibc/:
#           "$G/ld-linux-x86-64.so.2 --library-path $G .../tether_udp.dynamic.host.elf"
# Logs go next to the scripts (*.log, gitignored), the qemu rig lives
# in qemu/ (gitignored), see qemu_ahci.sh.
#   T       target ip, first argument of most scripts

HERE=$(cd "$(dirname "$0")" && pwd)
I686=${I686:-$HERE/../../i686}
TETHER=${TETHER:-$HERE/../tether_udp.dynamic.host.elf}
CON_DELAY=${CON_DELAY:-4}

# tether_udp send|plugin <file>, prints the result line
tu() {
    $TETHER "$1" "$T" "$2" 2>&1 | grep -E "complete|nb_missing|bad"
}
push_kernel() { tu send "$I686/kernel.3if"; sleep 5; }
push_plugin() { tu plugin "${1:-$I686/sqlite_plugin.bin}"; sleep 1; }

# Console: one command per argument, CON_DELAY apart, prints output
con() {
    { sleep 1; for c in "$@"; do printf "%s\n" "$c"; sleep $CON_DELAY; done; } |
        timeout $((2 + CON_DELAY * $#)) socat - UDP:$T:1001 2>/dev/null | tr -d "\r"
}

FAILED=0
check() {  # check <description> <command...>
    d="$1"; shift
    if "$@"; then echo "PASS $d"; else echo "FAIL $d"; FAILED=1; fi
}
finish() {
    [ $FAILED = 0 ] && echo "ALL PASS" || echo "SOME FAILED"
    exit $FAILED
}
