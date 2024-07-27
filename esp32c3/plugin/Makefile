# Tool prefix
TPF := riscv32-esp-elf-

GCC     := $(TPF)gcc
OBJDUMP := $(TPF)objdump
OBJCOPY := $(TPF)objcopy

CFLAGS := \


.PHONY: all clean

all: test.bin

clean:
	rm -f *.elf *.o *.bin *~

%.o: %.c
	$(GCC) -Os -o $@ -c $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.elf: %.o
	$(GCC) --static -nostartfiles -Tsram.ld -o $@ $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@
	hexdump -C $@




