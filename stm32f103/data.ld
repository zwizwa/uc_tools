/* Define sections. */
SECTIONS {
        /* Date store block. Can be loaded independently of app. */
        .store 0x8030000 : { 
                KEEP (*(.store_header))
                KEEP (*(.store_data))
                . = ALIGN(4);
        }
}


PROVIDE(_store = 0x08030000);

