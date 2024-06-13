#!/bin/sh
# FIXME: model after x8.f103.ld.sh - this is just a verbatim copy of
# old linker script.
cat <<EOF
/* Locally modified version of
   libopencm3/lib/libopencm3_stm32f1.ld */

MEMORY /* STM32F103x8 */
{
	ram (rwx) : ORIGIN = 0x20002000, LENGTH = 0x3000 /* 20kB total */
}

/* Define sections. */
SECTIONS
{
	.text : {
		_stext = .;
                KEEP (*(.run))     /* Startup code goes first. */
		*(.text*)          /* Program code */
                KEEP (*(.keep))    /* Code that might not be accessible */
		. = ALIGN(4);
		*(.rodata*)        /* Read-only data */
		. = ALIGN(4);
	} >ram

	/* C++ Static constructors/destructors, also used for __attribute__
	 * ((constructor)) and the likes */
	.preinit_array : {
		. = ALIGN(4);
		__preinit_array_start = .;
		KEEP (*(.preinit_array))
		__preinit_array_end = .;
	} >ram
	.init_array : {
		. = ALIGN(4);
		__init_array_start = .;
		KEEP (*(SORT(.init_array.*)))
		KEEP (*(.init_array))
		__init_array_end = .;
	} >ram
	.fini_array : {
		. = ALIGN(4);
		__fini_array_start = .;
		KEEP (*(.fini_array))
		KEEP (*(SORT(.fini_array.*)))
		__fini_array_end = .;
	} >ram

	/*
	 * Another section used by C++ stuff, appears when using newlib with
	 * 64bit (long long) printf support
	 */
	.ARM.extab : {
		*(.ARM.extab*)
	} >ram
	.ARM.exidx : {
		__exidx_start = .;
		*(.ARM.exidx*)
		__exidx_end = .;
	} >ram

	. = ALIGN(4);
	_etext = .;

	.data : {
		_data = .;
		*(.data*)	/* Read-write initialized data */
		. = ALIGN(4);
		_edata = .;
	} >ram
	_data_loadaddr = LOADADDR(.data);

	.bss : {
		*(.bss*)	/* Read-write zero initialized data */
		*(COMMON)
		. = ALIGN(4);
		_ebss = .;
	} >ram

	/*
	 * The .eh_frame section appears to be used for C++ exception handling.
	 * You may need to fix this if you're using C++.
	 */
	/DISCARD/ : { *(.eh_frame) }

	. = ALIGN(4);
	end = .;


}

SECTIONS {
        /* Symbolic names for register blocks. */
        .mem.40 0x40000000 (NOLOAD) : { KEEP (*(.mem_40*)) }
        .mem.50 0x50000000 (NOLOAD) : { KEEP (*(.mem_50*)) }
        .mem.E0 0xE0000000 (NOLOAD) : { KEEP (*(.mem_E0*)) }
}

PROVIDE(_stack = ORIGIN(ram) + LENGTH(ram));

EOF

