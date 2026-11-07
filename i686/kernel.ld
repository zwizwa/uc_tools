ENTRY(kmain)
SECTIONS {
    . = 0x7E00;  /* Start of kernel. */
    .text   : { *(.config) *(.text*) }
    .rodata : { *(.rodata*) }
    .data   : { *(.data*) }
    __data_end = .;
    /* Floppy loader reads 64 tracks of 9216 bytes from 0x7C00. */
    ASSERT(__data_end <= 0x7C00 + 64 * 9216, "kernel too big for floppy loader")
    /* Above 1MB, out of the way of sqlite.  A20 is on.
       Stack grows down from 16MB, 3if images load at 16MB. */
    .bss 0x100000 : {
        __bss_start = .;
        *(.bss*) *(COMMON)
        __bss_end = .;
    }
    ASSERT(__bss_end <= 0x1000000 - 0x100000, "bss leaves less than 1MB stack")
}
