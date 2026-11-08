#!/bin/sh
# Answers the kernel's `update` word (udp 1002 to boot_config.update):
# pushes kernel.3if, then the sender's plugin from apps.csv.
#   update_daemon.sh       as root on psamathe, runs until killed
HERE=$(cd "$(dirname "$0")" && pwd)
exec socat -u UDP-RECVFROM:1002,fork SYSTEM:"KERNEL=1 $HERE/load_plugin.sh \$SOCAT_PEERADDR"
