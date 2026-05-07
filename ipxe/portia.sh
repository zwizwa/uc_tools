#!/bin/bash
# iPXE with embedded script to break boot loop when loading from dnsmasq
make -C src bin/ipxe.hd EMBED=../zwizwa.ipxe && \
ls -l src/bin/ipxe.hd

