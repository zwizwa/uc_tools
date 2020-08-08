#!/bin/bash
[ -z "$2" ] && echo "usage: $0 <command> <channel_name> [<arg>]" >&2 && exit 1

# Simple communication channels based on named pipes, for use in shell
# scripts.  Note that this is essentially a hack, and the semantics
# are a little tricky due to several things being conflated:
#
# - The idea of "a line"
# - Bash's "read" function buffering more than a single line behind the scenes.
# - A session, e.g. what happens between writer opening and closing the pipe.

# For better defined semantics, see packet_bridge.

CMD=$1
PIPE=$2

shift ; shift

case $CMD in
    read)
        mkfifo "$PIPE" 2>/dev/null
        read line <"$PIPE"
        echo "$line"
        ;;
    write)
        mkfifo "$PIPE" 2>/dev/null
        echo "$@" >"$PIPE"
        ;;
    write_if_exists)
        [ ! -p "$PIPE" ] && exit 0
        shift ; shift
        echo "$@" >"$PIPE"
        ;;
    *)
        echo "bad command $1" >&2
        exit 1
        ;;
esac

