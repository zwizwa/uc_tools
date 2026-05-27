#!/bin/sh

# Connect picocom or other program to qemu's COM1
PICOCOM="picocom -q"
[ ! -z "$1" ] && PICOCOM="$1"

# I tried many combinations, eventually settled on:
# - tmux with pane-exited -> kill-session
# - qemu serial port on unix socket + socat PTY bridge for picocom

# The "sleep $WORKARKOUND" is a workaround for an urxvt bug not
# properly handling OSC 10/11 queries. Running in xterm doesn't have
# this problem.



cd $(dirname "$0")
# Make these local to the machine
SOCK=/tmp/com1.sock
PTY=/tmp/com1.pty
# Remove stale references
rm -f "$PTY" "$SOCK"

SLEEP=0.1

export SESSION=qemu-$$

cleanup() {
    tmux kill-session -t $SESSION
}

# Kill the tmux session also if the xterm is killed.
trap cleanup SIGHUP


WORKAROUND=0
if [ "$TERM" == rxvt-unicode-256color ]; then
    # urxvt OSC 10/11 bug workaround
    WORKAROUND=2
fi

tmux new-session -d -s $SESSION "exec socat PTY,link=$PTY,raw,echo=0 EXEC:'./qemu.sh -serial stdio'"
tmux set-hook -t $SESSION pane-exited "kill-session -t $SESSION"
tmux split-window -t $SESSION "while [ ! -e $PTY ]; do sleep $SLEEP; done; sleep $WORKAROUND; clear; set -x; exec $PICOCOM $PTY"
tmux select-layout -t $SESSION  even-vertical 
tmux resize-pane -t $SESSION -Z
tmux attach -t $SESSION

