#!/bin/sh
cd $(dirname "$0")
set -x
./graphviz.lua | dot -Tsvg >test_graphviz.svg


