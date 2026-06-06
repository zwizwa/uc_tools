#!/bin/sh
# Small boot floppy for National Semiconductor DP83815 Netgear / Soekris cards
# PCI ID 100b:0020
cd $(dirname "$0")
# kpxe=keep pxe=native
IMG=undionly.kpxe
# IMG=ipxe.pxe
./build.sh $IMG zwizwa.ipxe
ls -l src/bin/$IMG
rsync.clone src/bin/$IMG root@10.1.3.1:/etc/net/tftpboot/natsemi.kpxe
