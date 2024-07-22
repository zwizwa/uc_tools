# Tool prefix
TPF := xtensa-esp32-elf-

GCC := $(TPF)gcc
OBJDUMP := $(TPF)objdump


.PHONY: all clean

all: test.elf

clean:
	rm -f *.elf *.o *~

%.o: %.c
	$(GCC) -o $@ -c $<
	$(OBJDUMP) -d $@

%.elf: %.o
	$(GCC) -o $@ $<
	$(OBJDUMP) -d $@

