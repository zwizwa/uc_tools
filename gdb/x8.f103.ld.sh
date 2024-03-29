#!/bin/sh

# Generate the linker script.  The main reason to do it
# programatically is that INCLUDE doesn't work with out-of-tree
# builds.  So just flatten it.

[ -z "$MAIN_LD" ] && MAIN_LD=$(dirname $0)/stm32f1.ld.sh
[ -z "$END_LD" ]  && END_LD=$(dirname $0)/stm32f1_end.ld.sh 

cat <<EOF

/* GENERATED BY $(readlink -f $0) */

/* Note that all the STM32Fx8 chips we have seem to be xC chips with
   128kB intead of 64kB, so LENGTH is set to 1D000 instead of D000.
   See also the x8a, x8b scripts. */

MEMORY /* STM32F103x8 */
{
	rom (rx)  : ORIGIN = 0x08003000, LENGTH = 0x1D000 /* 128kB total */
	ram (rwx) : ORIGIN = 0x20002000, LENGTH = 0x3000 /* 20kB total */
}

PROVIDE(_flash_top = 0x08010000);

/* This is to keep the linker happy.  main() is not used. */
PROVIDE(main = 0);

/* Fake entry point.  Apps are started manually through gdb RSP 
PROVIDE (_fake_reset_handler = 0);
ENTRY (_fake_reset_handler); */

ENTRY (reset_handler);


/* BEGIN: $MAIN_LD */

$($MAIN_LD)

/* END: $MAIN_LD */


/* BEGIN: $END_LD */

$($END_LD)

/* END: $END_LD */


EOF

