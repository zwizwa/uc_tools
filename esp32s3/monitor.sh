#!/bin/sh
cd $(dirname "$0")
ESPPORT=/dev/ttyACM0 idf.py monitor
