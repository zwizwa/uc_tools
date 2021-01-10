MEMORY {
        rom (rx)  : ORIGIN = 0x08004C00, LENGTH = 0xFFFFFFFF
        ram (rwx) : ORIGIN = 0x200028a8, LENGTH = 0xFFFFFFFF
}
INCLUDE uc_tools/gdb/stm32f1.ld
SECTIONS {
    .flash_pad : {
        . = ALIGN(1024);
        _eplugin = . ;     
     } >rom
}

