

MEMORY
{
        /* 32k at the end of SRAM0. */
	iram (rwx) : ORIGIN = 0x40390a00, LENGTH = 0x8000
        /* 32k at the end of SRAM1 mapped as DRAM */
	dram (rw)  : ORIGIN = 0x3fc9c300, LENGTH = 0x8000
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


