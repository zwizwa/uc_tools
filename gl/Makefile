# The WebGL build rules are in uc_tools/em/Makefile

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:


.PHONY: all clean
all: sdl.elf # index.html


DEPS := $(shell find -name '*.d')
-include $(DEPS)

UC_TOOLS=$(shell readlink -f ..)

clean:
	rm -f *.d *.o *~ *.elf mod_graph*.c

CC=gcc

CFLAGS=\
	-I$(UC_TOOLS) \
	$(shell pkg-config --cflags sdl2)

LDFLAGS=$(shell pkg-config --libs sdl2)

# Convert file to C array.
mod_%_vert.c: %.vert
	xxd -i $< $@
mod_%_frag.c: %.frag
	xxd -i $< $@



%.o: %.c mod_graph_vert.c mod_graph_frag.c
	$(CC) $(CFLAGS) -MMD -MF $(@:.o=.d) -c $< -o $@

OBJS=graph.o
sdl.elf: $(OBJS)
	$(CC) -o $@ $(LDFLAGS) -lGLESv2 -lm $(OBJS)

.PHONY: info
info:
	pkg-config --libs sdl2

# python3 -m http.server 8000


# for xxd -i : generate header file from data for shaders, fonts
