
/* GENERATED BY /home/tom/exo/deps/uc_tools/gdb/x8.data.ld.sh */

SECTIONS {
        /* Date store block. Can be loaded independently of app. */
        .data 0x8008000 : { 
                KEEP (*(.data))
                . = ALIGN(4);
        }
}

PROVIDE(_data = 0x8008000);

