#!/bin/sh
# Run the test_*.sh tests that apply to a target.  As root on the
# host that reaches the target on-link (psamathe), see lib.sh for I686
# and TETHER.
#   test_run_all.sh qemu <qemu dir>  emulated rig, starts and stops
#                                    qemu, usually qemu/ next to this
#   test_run_all.sh x230 <ip>        ahci disk with a 7f partition, sqlite
#   test_run_all.sh rtl8139 <ip>     real rtl8139 card
# OTHER_PLUGIN=<sqlite_plugin.bin built for another kernel> adds the
# plugin mismatch test.  Prints PASS/FAIL lines, exits 1 on any FAIL.
HERE=$(cd "$(dirname "$0")" && pwd)
. "$HERE/lib.sh"
[ -n "$2" ] || { echo "usage: $0 qemu|x230|rtl8139 <qemu dir | ip>"; exit 1; }

out=$HERE/test_run_all.log
: >$out
run() {
    echo "## $*"
    "$HERE/$1.sh" "$2" ${3:+"$3"} 2>&1 | grep -a -E "PASS|FAIL|lines |hello! on" | tee -a $out
}
mismatch() {
    [ -n "$OTHER_PLUGIN" ] && run test_plugin_mismatch $1 "$OTHER_PLUGIN"
}
vm_stop() {
    kill $(pgrep -f "^qemu-system-i386 -name sqvfs") 2>/dev/null
    while pgrep -f "^qemu-system-i386 -name sqvfs" >/dev/null; do sleep 0.5; done
}

case "$1" in
qemu)
    Q=$2
    vm_stop
    run test_com1_tx "$Q"
    (cd "$Q" && DBG=1 NIC=rtl8139 nohup "$HERE/qemu_ahci.sh" >qsq.out 2>&1 </dev/null &)
    sleep 6
    T=10.1.54.222
    run test_3if_integrity "$Q"
    for t in test_isr_log test_sqlite test_rtl8139_flood test_kq_burst test_timer; do run $t $T; done
    run test_tcp_console $T 10.1.54.9  # routed tap, see the script
    run test_http $T 10.1.54.9
    mismatch $T
    vm_stop
    ;;
x230)
    T=$2
    push_kernel >/dev/null
    for t in test_isr_log test_sqlite test_kq_burst test_timer; do run $t $T; done
    mismatch $T
    ;;
rtl8139)
    T=$2
    push_kernel >/dev/null
    for t in test_isr_log test_kq_burst test_rtl8139_flood test_timer; do run $t $T; done
    ;;
*)
    echo "unknown target $1"; exit 1
    ;;
esac

n=$(grep -c FAIL $out)
echo "## $(grep -c PASS $out) PASS, $n FAIL"
[ $n = 0 ]
