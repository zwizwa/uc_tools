# Isolated Makefile + host.nix to build uc_tools/linux files.

# Part of transition from one big makefile to "build environment per
# directory", which makes it easier to keep things separated.

UC_TOOLS := ..
CFLAGS := \
	-I$(UC_TOOLS) \
	-Wall -Werror \

CC := gcc

ELF := \
	test_lru.elf \
	timeseries.elf \



MAKEFLAGS += --no-builtin-rules
.SHELLFLAGS := -o pipefail -c
.SUFFIXES:


.PHONY: all clean
all: $(ELF)

DEPS := $(shell find -name '*.d')
-include $(DEPS)

clean:
	rm -f *.o *.elf *.d

%.o: ../linux/%.c
	$(CC) $(CFLAGS) -MMD -MF $(@:.o=.d) -o $@ -c $<
	ls -l $@

%.elf: %.o Makefile
	$(CC) -o $@ $<
	ls -l $@
