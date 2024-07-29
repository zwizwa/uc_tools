/* To be used with ram.ld.sh */

SECTIONS
{
        /* Support store header. See store.ld */
        .store : { 
                KEEP (*(.store_header))
                KEEP (*(.store_data))
                . = ALIGN(4);
        } >ram
        /* Regular code and data, including KEEP roots for ld gc. */
        .text : {
                *(.text*)
                KEEP (*(.keep))
                . = ALIGN(4);
        } >ram
        .data : {
                *(.rodata*)
                . = ALIGN(4);
                *(.data*)
                . = ALIGN(4);
        } >ram
        .bss : {
                *(.bss*)
                . = ALIGN(4);
        } >ram
}
ENTRY(run);

