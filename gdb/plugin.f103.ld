MEMORY {
        rom (rx)  : ORIGIN = 0x08004400, LENGTH = 0xFFFFFFFF
        ram (rwx) : ORIGIN = 0x2000288c, LENGTH = 0xFFFFFFFF
}
INCLUDE uc_tools/gdb/stm32f1.ld
SECTIONS {
    .flash_pad : {
        . = ALIGN(1024);
        _eplugin = . ;     
     } >rom
}

