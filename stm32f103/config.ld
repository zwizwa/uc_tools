/* Define sections. */
SECTIONS {
        /* Configuration block.
           This has a default in core and might be overwritten by app. */
        .config 0x8002800 : { 
                KEEP (*(.config_header))
                KEEP (*(.config_data))
                . = ALIGN(4);
        }
         	

                

        /* Symbolic names for register blocks. */
        .mem.40 0x40000000 (NOLOAD) : { KEEP (*(.mem_40*)) }
        .mem.50 0x50000000 (NOLOAD) : { KEEP (*(.mem_50*)) }
        .mem.E0 0xE0000000 (NOLOAD) : { KEEP (*(.mem_E0*)) }
}

/* Boot loader config block. */
PROVIDE(_config = 0x08002800);


