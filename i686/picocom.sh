#!/bin/sh

# Connect picocom to qemu's COM1

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

tmux1 () {

WORKAROUND=1
tmux new-session -d -s $SESSION "./qemu.sh -serial unix:$SOCK,server"
tmux set-hook -t $SESSION pane-exited "kill-session -t $SESSION"
tmux split-window -t $SESSION "while [ ! -e $SOCK ]; do sleep $SLEEP; done; socat PTY,link=$PTY,raw,echo=0 UNIX-CONNECT:$SOCK"
tmux split-window -t $SESSION "while [ ! -e $PTY ]; do sleep $SLEEP; done; sleep $WORKAROUND; clear; exec picocom $PTY"
tmux select-layout -t $SESSION  even-vertical 
tmux resize-pane -t $SESSION -Z
tmux attach -t $SESSION

}

tmux2 () {

# FIXME: If (old?) urxvt is detected, this should be set to 2.
WORKAROUND=0

tmux new-session -d -s $SESSION "exec socat PTY,link=$PTY,raw,echo=0 EXEC:'./qemu.sh -serial stdio'"
tmux set-hook -t $SESSION pane-exited "kill-session -t $SESSION"
# tmux set -t $SESSION escape-time 2000
tmux split-window -t $SESSION "set -x ; while [ ! -e $PTY ]; do sleep $SLEEP; done; sleep $WORKAROUND; echo; echo $TERM; exec picocom $PTY"
tmux select-layout -t $SESSION  even-vertical 
tmux resize-pane -t $SESSION -Z
tmux attach -t $SESSION

}


#tmux1
tmux2

