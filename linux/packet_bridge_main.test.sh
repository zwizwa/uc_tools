#!/bin/bash
ABSPATH="/i/panda/$(readlink -f $(dirname $0))"
exec ssh zoe $ABSPATH/packet_bridge_main.dynamic.host.elf - USB
