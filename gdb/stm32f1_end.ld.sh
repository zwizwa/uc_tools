#!/bin/sh

# Postable used for firmware images.
# Do not use this in the bootloader.

# Default for 128k Flash devices
# For 256k Flash it is 2048
[ -z "$FLASH_BLOCK_SIZE" ] && FLASH_BLOCK_SIZE=1024

cat <<EOF

SECTIONS {

        /* In previous versions, _eflash pointed to the end of Flash
           memory, which used to be the start of the region that the
           application could use to write to the Flash.  It was also
           used as endx to compute the firmware image CRC.  However,
           the contents of the padded memory is not defined, so for
           CRC we now usethe symbol name is changed to _flash_endx and
           not rounded.  Semantics changed, so we're using new
           names. */

 	.firmware_end : {
		_firmware_endx = . ;     
		. = ALIGN($FLASH_BLOCK_SIZE);
		_firmware_block_endx = . ;     
 	} >rom

        /* If there is a .control section in the .o files, then
           reserve a control block following the firmware blocks that
           can later be patched by objcopy --update-section.  This is
           used e.g. by elf2fw.sh to store CRC data in the .elf, which
           can only be computed in a second pass after linking has
           finished. */

        .control : {
		_control = . ;
                KEEP (*(.control)) ;
		. = ALIGN($FLASH_BLOCK_SIZE);
        } >rom

        .flash_free : {
                _flash_free = . ;
        } >rom

}

EOF
