#!/bin/sh
cd $(dirname "$0")
../linux/tether_udp.dynamic.host.elf send 10.1.54.222 kernel.3if
