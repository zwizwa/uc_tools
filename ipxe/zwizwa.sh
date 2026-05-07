#!/bin/bash
# iPXE with embedded script to break boot loop when loading from dnsmasq
make -C src bin/undionly.kpxe EMBED=../zwizwa.ipxe && \
scp src/bin/undionly.kpxe 10.1.3.2:/tmp/

