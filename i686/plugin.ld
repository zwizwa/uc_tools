/* Plugin at PLUGIN_ADDR, see plugin.h.  Link with -R kernel.elf. */
ENTRY(plugin_header)
SECTIONS {
    . = 0x2000000;
    /* struct plugin_header first */
    .text   : { KEEP(*(.plugin_header)) *(.text*) }
    .rodata : { *(.rodata*) }
    .data   : { *(.data*) }
    /* Not in the image, the kernel clears it */
    .bss    : {
        __plugin_bss_start = .;
        *(.bss*) *(COMMON)
        __plugin_bss_end = .;
    }
}
