#!/bin/sh
cd $(dirname "$0")
mkdir -p build
cd build
exec cmake .. -G Ninja
