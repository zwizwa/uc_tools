#!/bin/sh

HERE=$(dirname $0)


if [ ! -e .git ]; then
    echo "00000000-$(date '+%Y%m%d-%H%M')"
    exit 0
fi

v="$(cd $HERE ; git rev-parse --short=8 HEAD)-$(date '+%Y%m%d-%H%M')"
if [ ! -z "$(cd $HERE ; git diff)" ] ; then
    # When dirty, always call it dirty.
    v="$v-dirty"
    echo $v
    exit 0
fi

if [ "$SOFTWARE_VERSION" ]; then
    # rdm-bridge-releases will set this.  we can use a shorter time stamp
    v="$(cd $HERE ; git rev-parse --short=8 HEAD)-$(date '+%Y%m%d')-${SOFTWARE_VERSION}"
    echo $v
    exit 0
fi
    
echo $v
