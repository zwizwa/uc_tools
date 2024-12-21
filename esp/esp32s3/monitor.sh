#!/bin/sh
cd $(dirname "$0")
. ./espport.sh
exec ../run.sh idf.py monitor
