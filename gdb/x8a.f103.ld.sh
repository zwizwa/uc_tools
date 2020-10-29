#!/bin/bash

# Variant of x8, for use with dual-partition firmware on 128k Flash STM32F103
#
# gdbstub:     0x08000000 - 0x08003000
# trampoline:  0x08003000 - 0x08004000
# partition a: 0x08004000 - 0x08012000 (both have 0x800 config + rest is firmware)
# partition b: 0x08012000 - 0x08020000

# Partitions might need to move if the trampoline doesn't fit: it has
# a CRC routine.

# See x8.f103.ld.sh for more information.

[ -z "$MAIN_LD" ] && MAIN_LD=$(dirname $0)/stm32f1.ld.sh

case $(basename $0) in
    x8b.f103.ld.sh)
        CONFIG=0x08012000
        ORIGIN=0x08012800
        ;;
    *)
        CONFIG=0x08004000
        ORIGIN=0x08004800
        ;;
esac

export CONFIG
export ORIGIN

# echo ORIGIN=$ORIGIN >&2

cat <<EOF


/* GENERATED BY $(readlink -f $0) */

MEMORY /* STM32F103x8 */
{
        rom (rx)  : ORIGIN = $ORIGIN, LENGTH = 0xD800
	ram (rwx) : ORIGIN = 0x20002000, LENGTH = 0x3000 /* 20kB total */
}

/* Most stm32f103 in the wild have 128k */
PROVIDE(_flash_top = 0x08020000);

/* This is to keep the linker happy.  main() is not used. */
PROVIDE(main = 0);

/* Fake entry point.  Apps are started manually through gdb RSP 
PROVIDE (_fake_reset_handler = 0);
ENTRY (_fake_reset_handler); */

ENTRY (reset_handler);


/* BEGIN: Include the common ld script. */

$($MAIN_LD)

/* END: Include the common ld script. */  

/* FIXME: Can't put this in the bootloader.  Is it ok to sit here
after the .config and .mem.* sections? */

SECTIONS {
	/* _eflash:         first free block in Flash memory for application use.
	   _flash_bin_endx: end of firmware binary */
 	.flash_pad : {
		_flash_bin_endx = . ;     
		. = ALIGN(1024);
		_eflash = . ;     
 	} >rom

        /* If there is a .control section, then reserve a control
           block at _eflash that can later be patched by objcopy
           --update-section.  This is used e.g. by
           patch-control-block.sh to store CRC data in the .elf, which
           can only be computed in a second pass after linking has
           finished. */

        .control : {
		_control = . ;
                KEEP (*(.control)) ;
		. = ALIGN(1024);
        } >rom
}

EOF

