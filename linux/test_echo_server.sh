#!/bin/bash
exec $(dirname $0)/packet_bridge_main.dynamic.host.elf \
	HEX EXEC:4:./netcat_1234.sh
