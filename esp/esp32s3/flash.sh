#!/bin/sh
cd $(dirname "$0")
. ./espport.sh
./ninja.sh flash
