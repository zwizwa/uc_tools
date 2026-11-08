#!/bin/sh
# Kernel console over UDP, Enter connects and shows unread log.
# Needs to run on the same subnet, the kernel has no gateway.
exec socat - UDP:${1:-10.1.9.222}:1001
