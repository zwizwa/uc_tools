#!/bin/bash

# If VERSION is not defined as environment variable, compute a version
# string from the current time, the git revision and whether the
# repository is clean or not.

HERE=$(dirname $0)
if [ -z "$VERSION" ]; then
    if [ ! -f $HERE/.git ]; then
        VERSION="unknown"
    else
        v="$(date '+%Y%m%d-%H%M%S')-$(cd $HERE ; git rev-parse --short=6 HEAD)-dev"
        if [ -z "$(cd $HERE ; git diff)" ] ; then
            VERSION="$v"
        else
            VERSION="$v-dirty"
        fi
    fi
fi
echo $VERSION
