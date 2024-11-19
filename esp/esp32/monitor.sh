#!/bin/sh
cd $(dirname "$0")
. ./espport.sh
idf.py monitor
