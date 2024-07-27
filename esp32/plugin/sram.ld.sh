#!/bin/sh

# Get the memory info from the device.
CMD="/i/tom/rdm-bridge/tools/tether_bl.dynamic.host.elf 192.168.6.122"
MEMINFO=$(dirname "$0")/meminfo.sh
$CMD meminfo >$MEMINFO
cat $MEMINFO
. $MEMINFO

# [ -z "$IRAM_ADDR" ] && IRAM_ADDR=0x40098000
# [ -z "$DRAM_ADDR" ] && DRAM_ADDR=0x3FFF8000
# [ -z "$IRAM_LEN"  ] && IRAM_LEN=0x8000
# [ -z "$DRAM_LEN"  ] && DRAM_LEN=0x8000

# xtensa-esp32-elf-gcc -mtext-section-literals --static -nostartfiles -Tsram.ld
# xtensa-esp32-elf-objcopy -O binary --only-section=.iram ${PLUGIN}.elf ${PLUGIN}.iram.bin
# xtensa-esp32-elf-objcopy -O binary --only-section=.dram ${PLUGIN}.elf ${PLUGIN}.dram.bin


cat <<EOF


MEMORY
{
        /* 32k at the end of SRAM0. */
	iram (rwx) : ORIGIN = ${FLASH_ADDR}, LENGTH = ${FLASH_LEN}
        /* 32k at the end of SRAM1 mapped as DRAM */
	dram (rw)  : ORIGIN = ${RAM_ADDR}, LENGTH = ${RAM_LEN}
}

/* Define sections. */
SECTIONS
{
	.iram : {
		_stext = .;
                KEEP (*(.run))     /* Startup trampoline goes first.  Should not contain l32r instructions. */
		_sliteral = .;
                KEEP (*(.literal)) /* Literals need to come before the code referencing it if l32r instruction is used. */
		. = ALIGN(4);
		*(.text*)          /* Program code */
		. = ALIGN(4);
	} >iram

	.dram : {
		*(.rodata*)        /* Read-only data */
		. = ALIGN(4);

		_data = .;
		*(.data*)	/* Read-write initialized data */
		. = ALIGN(4);
		_edata = .;

		*(.bss*)	/* Read-write zero initialized data */
		*(COMMON)
		. = ALIGN(4);
		_ebss = .;

	} >dram
}


EOF
