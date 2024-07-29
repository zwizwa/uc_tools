#ifndef OLED_H
#define OLED_H

/* This is init code for HiLetgo 1.3" SPI 128x64 SH1106 OLED module.

   SH1106 datasheet doesn't mention max frequency.  Alledgedly 12MHz
   is ok.  We can do 18 or 9 on STM32F103.  Let's stick to 9 for
   breadboard operation, which is DIV_8.  As for clock polarity,
   timing diagram suggests

   1WR 1=clock idle, followed by falling write and rising read.

   SH1106 uses the DC signal to select between data=1 or command=1
   register write.  DC (A0) is latched together with the last bit in
   an 8 bit SPI transfer.

   Clock mode: idle is high, read ege is the rising edge, so we write
   on the falling edge, i.e following our custom notation, mode=1WR

   The display needs to be reset before initialization by bringing the
   reset pin low for 10 ms before sending the configuration commands
   with DC=0.  This is cargo-culted from example code.

   Data is sent to the device one page at a time.  A page is 8 pixels
   high.  Before each page, a couple of bytes are sent in command
   mode.  Example code left 1 us between command and data packets but
   we do not seem to need that.

   This code uses 4-WIRE SPI mode, as 3-WIRE use non-standard 9 bit
   bytes which are a pain to use.  This makes DMA more difficult, but
   DMA is hard to do anyway with multiple page transfers, so oled.c
   uses busywait in the main loop.

*/



// Commands are sent with DC=0
// Pull reset low for 10 ms before sending these in command mode.
#define OLED_INIT(CMD,contrast) \
    CMD( 0xAE ) /* panel off */                                             \
    CMD( 0x02 ) /* low column address */                                    \
    CMD( 0x10 ) /* high column address */                                   \
    CMD( 0x40 ) /* start line address (Mapping RAM Display Start Line (0x00~0x3F)) */ \
    CMD( 0xA1 ) /* SEG/Column Mapping */                                    \
    CMD( 0xC8 ) /* COM/Row Scan Direction */                                \
    CMD( 0xA6 ) /* normal display */                                        \
    CMD( 0xA8 ) /* multiplex ratio(1 to 64) */                              \
    CMD( 0x3F ) /* 1/64 duty */                                             \
    CMD( 0xD3 ) /* display offset (shift Mapping RAM Counter (0x00~0x3F)) */ \
    CMD( 0x00 ) /* not offset */                                            \
    CMD( 0xd5 ) /* display clock divide ratio/oscillator frequency */       \
    CMD( 0x80 ) /* divide ratio, Clock as 100 Frames/Sec */                 \
    CMD( 0xAD ) /* charge pump */                                           \
    CMD( 0x8B ) /* Charge Pump enable */                                    \
    CMD( 0xDA ) /* com pins hardware configuration */                       \
    CMD( 0x12 )                                                             \
    CMD( 0x81 ) /* contrast control register */                             \
    CMD( contrast )                                                         \
    CMD( 0xD9 ) /* pre-charge period */                                     \
    CMD( 0x22 ) /* Pre-Charge */                                            \
    CMD( 0xDB ) /* VCOM */                                                  \
    CMD( 0x40 ) /* VCOM Deselect Level */                                   \
    CMD( 0x20 ) /* Page Addressing Mode (0x00/0x01/0x02) */                 \
    CMD( 0x02 ) /* */                                                       \
    CMD( 0xA4 ) /* disable Entire Display On (0xA4/0xA5) */                 \
    CMD( 0xA6 ) /* disable Inverse Display On (0xA6/A7) */                  \
    CMD( 0xAF ) /* turn on oled panel */                                    \

// Send with DC=0, then follow up data with DC=1.  Note that columns
// have been offset by 2, which seems to be the first visible column
// on this module.
#define OLED_PAGE(CMD,p,c)                                           \
    CMD(0xB0 + ((p)&0xF))       /* page address */                   \
    CMD(0x02 + ((c)&0xF))       /* set low column address */         \
    CMD(0x10 + (((c)>>16)&0xF)) /* set high column address */

#endif
