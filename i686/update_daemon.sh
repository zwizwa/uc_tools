#!/bin/sh
# Answers the kernel's `update` word (udp 1002 to boot_config.update):
# pushes kernel.3if, then the sender's plugin from apps.csv.
#   update_daemon.sh       as root on psamathe, runs until killed
# - copied into etc_net by nix/pkgs/etc_net/update_from_uc_tools.sh, runs
#   there as psamathe's uc-tools-update systemd unit
HERE=$(cd "$(dirname "$0")" && pwd)
exec socat -u UDP-RECVFROM:1002,fork SYSTEM:"KERNEL=1 $HERE/load_plugin.sh \$SOCAT_PEERADDR"
