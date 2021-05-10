#!/bin/bash
HERE=$(dirname $0)
WEBROOT=$(readlink -f $HERE/../webroot)
BUFFER=/tmp/la_ws.bin
NB_SLICES=60
TCP_PORT=4567
/i/exo/logan/dev/saleae.sh | ./la_ws.dynamic.host.elf $WEBROOT $BUFFER $NB_SLICES $TCP_PORT

