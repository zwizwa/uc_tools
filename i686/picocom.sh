#!/bin/sh

# I tried many combinations, eventually settled on:
# - tmux with pane-exited -> kill-session
# - qemu serial port on unix socket + socat PTY bridge for picocom

# The "sleep $WORKARKOUND" before starting picocom: something is
# sending an escape code to that terminal and I don't know where it
# comes from.  The sleep avoids it going to the emulator's serial
# port.
WORKAROUND=1


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

tmux new-session -d -s $SESSION "set -x ; ./qemu.sh -serial unix:$SOCK,server; sleep 1000"
tmux set-hook -t $SESSION pane-exited "kill-session -t $SESSION"
tmux split-window -t $SESSION "set -x; while [ ! -e $SOCK ]; do sleep $SLEEP; done; socat PTY,link=$PTY,raw,echo=0 UNIX-CONNECT:$SOCK; sleep 1000"
tmux split-window -t $SESSION "set -x; while [ ! -e $PTY ]; do sleep $SLEEP; done; sleep $WORKAROUND; exec picocom $PTY"
tmux select-layout -t $SESSION  even-vertical 
tmux resize-pane -t $SESSION -Z
tmux attach -t $SESSION

cleanup
