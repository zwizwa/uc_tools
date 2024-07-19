# Tool prefix
TPF := xtensa-esp32-elf-

GCC := $(TPF)gcc
OBJDUMP := $(TPF)objdump


.PHONY: all clean

all: test.elf

clean:
	rm -f *.elf *~

%.elf: %.c
	$(GCC) -o $@ $<
	$(OBJDUMP) -d $@

