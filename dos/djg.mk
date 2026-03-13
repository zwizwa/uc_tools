# See /i/exo/nix/cross/cross.nix
CROSS=~/.result/cross
CC=$(CROSS)/bin/i586-pc-msdosdjgpp-gcc
NASM=$(CROSS)/bin/nasm

# Note that DJGPP is built around DPMI protected mode interface, e.g. CWSDPMI.EXE
# http://www.delorie.com/pub/djgpp/current/v2misc/csdpmi7b.zip
# https://en.wikipedia.org/wiki/DOS_Protected_Mode_Interface

%.exe: %.c
	$(CC) -o $@ $<

