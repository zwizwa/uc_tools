# See /i/exo/nix/cross/cross.nix
BIN=~/.result/cross/bin

%.exe: %.c watcom.mk
	$(BIN)/wcl -za99 -fe=$@ $<
#	$(BIN)/owcc -std=c99 -o $@ $< # didn't work


