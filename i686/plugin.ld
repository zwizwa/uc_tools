/* Plugin at PLUGIN_ADDR, see plugin.h.  Link with -R kernel.elf. */
ENTRY(plugin_start)
SECTIONS {
    . = 0x2000000;
    /* 3if JSR jumps to the first byte */
    .text   : { *(.plugin_start) *(.text*) }
    .rodata : { *(.rodata*) }
    .data   : { *(.data*) }
    /* Not in the image, plugin_start clears it */
    .bss    : {
        __plugin_bss_start = .;
        *(.bss*) *(COMMON)
        __plugin_bss_end = .;
    }
}
