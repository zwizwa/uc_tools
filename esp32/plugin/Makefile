# Tool prefix
TPF := xtensa-esp32-elf-

GCC     := $(TPF)gcc
OBJDUMP := $(TPF)objdump
OBJCOPY := $(TPF)objcopy

CFLAGS := \


.PHONY: all clean

all: test.iram.bin test.dram.bin

clean:
	rm -f *.elf *.o *.bin *~

%.o: %.c
	$(GCC) -mtext-section-literals -Os -o $@ -c $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.elf: %.o
	$(GCC) --static -nostartfiles -Tsram.ld -o $@ $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.iram.bin: %.elf
	$(OBJCOPY) -O binary --only-section=.iram $< $@
	hexdump -C $@

%.dram.bin: %.elf
	$(OBJCOPY) -O binary --only-section=.dram $< $@
	hexdump -C $@



