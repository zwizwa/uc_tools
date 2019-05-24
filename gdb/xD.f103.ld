MEMORY /* STM32F103xD */
{
	rom (rx)  : ORIGIN = 0x08003000, LENGTH = 0x2D000 /* 384kB total (192kB store) */
	ram (rwx) : ORIGIN = 0x20002000, LENGTH =  0xE000 /*  64kB total */
}

PROVIDE(_flash_top = 0x08060000);

/* Fake entry point.  Apps are started manually through gdb RSP 
PROVIDE (_fake_reset_handler = 0);
ENTRY (_fake_reset_handler); */

ENTRY (reset_handler);

/* Include the common ld script. */
INCLUDE stm32f1.ld



