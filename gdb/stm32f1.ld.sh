#!/bin/sh

[ -z "$CONFIG" ] && CONFIG=0x8002800

cat <<EOF

/* Locally modified version of
   libopencm3/lib/libopencm3_stm32f1.ld */

/*
 * This file is part of the libopencm3 project.
 *
 * Copyright (C) 2009 Uwe Hermann <uwe@hermann-uwe.de>
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Generic linker script for STM32 targets using libopencm3. */

/* Memory regions must be defined in the ld script which includes this one. */

/* Enforce emmition of the vector table. */
EXTERN (vector_table)

/* Define sections. */
SECTIONS
{
	.text : {
		_stext = .;
                KEEP (*(.plugin_header))  /* Start of Flash data in plugis. */
		*(.vectors)        /* Vector table */
                KEEP (*(.service)) /* Bootloader services */
		*(.text*)          /* Program code */
                KEEP (*(.keep))    /* Test code */
		. = ALIGN(4);
		*(.rodata*)        /* Read-only data */
		. = ALIGN(4);

                /* It is very convenient to be able to collect sets of
                objects that were included in a firmware image by the
                the compilation and linking processes, without having
                to explicitly specify them.  This is currently used to
                track protocol symbol<->id maps and debug commands. */

                _symbol_start = .;                
                KEEP (*(.symbol))
                *(.symbol*)
                _symbol_endx = .;                

                _command_start = .;                
                KEEP (*(.command))
                *(.command*)
                _command_endx = .;                

		_eflash = .;

	} >rom

	/* C++ Static constructors/destructors, also used for __attribute__
	 * ((constructor)) and the likes */
	.preinit_array : {
		. = ALIGN(4);
		__preinit_array_start = .;
		KEEP (*(.preinit_array))
		__preinit_array_end = .;
	} >rom
	.init_array : {
		. = ALIGN(4);
		__init_array_start = .;
		KEEP (*(SORT(.init_array.*)))
		KEEP (*(.init_array))
		__init_array_end = .;
	} >rom
	.fini_array : {
		. = ALIGN(4);
		__fini_array_start = .;
		KEEP (*(.fini_array))
		KEEP (*(SORT(.fini_array.*)))
		__fini_array_end = .;
	} >rom

	/*
	 * Another section used by C++ stuff, appears when using newlib with
	 * 64bit (long long) printf support
	 */
	.ARM.extab : {
		*(.ARM.extab*)
	} >rom
	.ARM.exidx : {
		__exidx_start = .;
		*(.ARM.exidx*)
		__exidx_end = .;
	} >rom

	. = ALIGN(4);
	_etext = .;

	.data : {
		_data = .;
		*(.data*)	/* Read-write initialized data */
		. = ALIGN(4);
		_edata = .;
	} >ram AT >rom
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
        /* Configuration block.
           This has a default in core and might be overwritten by app. */
        .config $CONFIG : { 
                _firmware_start = .;
                KEEP (*(.config_header))
                KEEP (*(.config_data))
                . = ALIGN(0x800);
        }

        /* Symbolic names for register blocks. */
        .mem.40 0x40000000 (NOLOAD) : { KEEP (*(.mem_40*)) }
        .mem.50 0x50000000 (NOLOAD) : { KEEP (*(.mem_50*)) }
        .mem.E0 0xE0000000 (NOLOAD) : { KEEP (*(.mem_E0*)) }
}

PROVIDE(_stack = ORIGIN(ram) + LENGTH(ram));

/* Bootloader service functions. */
PROVIDE(_service = 0x08000150);

/* Boot loader config block. */
PROVIDE(_config = $CONFIG);

EOF

