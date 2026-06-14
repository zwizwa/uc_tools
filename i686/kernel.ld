ENTRY(kmain)
SECTIONS {
    . = 0x7E00;  /* Start of kernel. */
    .text   : { *(.config) *(.text) }
    .rodata : { *(.rodata) }
    .data   : { *(.data) }
    .bss    : {
        __bss_start = .;
        *(.bss) *(COMMON)
        __bss_end = .;
    }
}
