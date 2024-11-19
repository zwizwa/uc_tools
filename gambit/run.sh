#!/bin/sh
GSC=$(readlink -f $(which gsc))
DIR=$(dirname $(dirname $GSC))
echo DIR=$DIR >&2
export LD_LIBRARY_PATH=$DIR/lib
exec "$@"




