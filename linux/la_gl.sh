#!/bin/sh
# usage: la_gl.sh [<stream_program>]
# Overview: see la_gl.c
# Default stream is the Logic8.  Fake DMX: test_la_gl_dmx.dynamic.host.elf
HERE=$(dirname $(readlink -f $0))
BUFFER=/var/lib/la_gl/la_gl.bin
NB_SLICES=60
TCP_PORT=8091
mkdir -p $(dirname $BUFFER)
${1:-$HERE/sigrok.sh} | exec $HERE/la_gl.dynamic.host.elf $HERE/../webapp/la_gl $BUFFER $NB_SLICES $TCP_PORT
