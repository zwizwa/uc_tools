#ifndef HW_SWO_H
#define HW_SWO_H

/* Some notes about the trace port

   - The application side is straightforward: poll trace port ready
     bit, then write data to it.  Typically trace port 0 is used.

   - Setup is typically done by the debugger, e.g. see
     openocd/src/target/armv7m_trace.c - however I did not find this
     code in the blackmagic probe, so init code is here below.

   - Note that for an SWJ port (one that can do both JTAG and SWD and
     boots up as JTAG), the SWO output only becomes accessible _after_
     the debugginer switched to SWD using the magic sequence.

*/

#include <libopencm3/cm3/itm.h>
#include <libopencm3/cm3/scs.h>
#include <libopencm3/cm3/tpiu.h>
#include <libopencm3/stm32/dbgmcu.h>
// http://www.tmos-arm.com/doxy/d7/d08/a00378.html
#define TPIU_CSPSR_BYTE 0x0000000


/* I found 3 variants that perform the necessary debug register writes
   to enable the TPIO.  The first two are adapted from

   https://os.mbed.com/users/wim/code/SWO/docs/tip/SWO_8cpp_source.html

   The third one is from

   https://mcuoneclipse.com/2016/10/17/tutorial-using-single-wire-output-swo-with-arm-cortex-m-and-eclipse/
*/

void hw_swo_init_1(void) {
    /* Enable trace subsystem (we'll use ITM and TPIU) */
    SCS_DEMCR |= SCS_DEMCR_TRCENA;

    /* Use Manchester code for asynchronous transmission */
    TPIU_SPPR = TPIU_SPPR_ASYNC_MANCHESTER;
    TPIU_ACPR = 7;

    /* Data width is 1 byte */
    TPIU_CSPSR = TPIU_CSPSR_BYTE;

    /* Formatter and flush control */
    TPIU_FFCR &= ~TPIU_FFCR_ENFCONT;

    /* Enable TRACESWO pin for async mode */
    DBGMCU_CR = DBGMCU_CR_TRACE_IOEN | DBGMCU_CR_TRACE_MODE_ASYNC;

    /* Unlock access to ITM registers */
    /* FIXME: Magic numbers... Is this Cortex-M3 generic? */
    *((volatile uint32_t*)0xE0000FB0) = 0xC5ACCE55;

    /* Enable ITM with ID = 1 */
    ITM_TCR = (1 << 16) | ITM_TCR_ITMENA;
    /* Enable stimulus port 1 */
    ITM_TER[0] = 1;
}

void hw_swo_init_2(void) {
  uint32_t SWOPrescaler;
  uint32_t SWOSpeed;

  //<Init PLL, set CPU clock to 72 MHz>  // Optional, so I do not pos it here

  SWOSpeed = 600000;
  *((volatile unsigned *)0xE000EDFC) = 0x01000000;   // "Debug Exception and Monitor Control Register (DEMCR)"
  *((volatile unsigned *)0xE0042004) = 0x00000027;
  *((volatile unsigned *)0xE00400F0) = 0x00000002;   // "Selected PIN Protocol Register": Select which protocol to use for trace output (2: SWO)
  SWOPrescaler = (72000000 / SWOSpeed) - 1;  // SWOSpeed in Hz
  *((volatile unsigned *)0xE0040010) = SWOPrescaler; // "Async Clock Prescaler Register". Scale the baud rate of the asynchronous output
  *((volatile unsigned *)0xE0000FB0) = 0xC5ACCE55;   // ITM Lock Access Register, C5ACCE55 enables more write access to Control Register 0xE00 :: 0xFFC
  *((volatile unsigned *)0xE0000E80) = 0x0001000D;   // ITM Trace Control Register
  *((volatile unsigned *)0xE0000E40) = 0x0000000F;   // ITM Trace Privilege Register
  *((volatile unsigned *)0xE0000E00) = 0x00000001;   // ITM Trace Enable Register. Enabled tracing on stimulus ports. One bit per stimulus port.
  *((volatile unsigned *)0xE0001000) = 0x400003FE;   // DWT_CTRL
  *((volatile unsigned *)0xE0040304) = 0x00000100;   // Formatter and Flush Control Register
}



#if 1
// partially converted (FIXME)
void hw_swo_init_3(void) {

// https://mcuoneclipse.com/2016/10/17/tutorial-using-single-wire-output-swo-with-arm-cortex-m-and-eclipse/
/*!
 * \brief Initialize the SWO trace port for debug message printing
 * \param portBits Port bit mask to be configured
 * \param cpuCoreFreqHz CPU core clock frequency in Hz
 */

    uint32_t portBits = 1;
    uint32_t cpuCoreFreqHz = 72000000;

    uint32_t SWOSpeed = 64000; /* default 64k baud rate */
    uint32_t SWOPrescaler = (cpuCoreFreqHz / SWOSpeed) - 1; /* SWOSpeed in Hz, note that cpuCoreFreqHz is expected to be match the CPU core clock */
 
    SCS_DEMCR = SCS_DEMCR_TRCENA; /* enable trace in core debug */
    *((volatile unsigned *)(ITM_BASE + 0x400F0)) = 0x00000002; /* "Selected PIN Protocol Register": Select which protocol to use for trace output (2: SWO NRZ, 1: SWO Manchester encoding) */
    *((volatile unsigned *)(ITM_BASE + 0x40010)) = SWOPrescaler; /* "Async Clock Prescaler Register". Scale the baud rate of the asynchronous output */
    *((volatile unsigned *)(ITM_BASE + 0x00FB0)) = 0xC5ACCE55; /* ITM Lock Access Register, C5ACCE55 enables more write access to Control Register 0xE00 :: 0xFFC */
    ITM_TCR  = ITM_TCR_TRACE_BUS_ID_MASK | ITM_TCR_SWOENA | ITM_TCR_SYNCENA | ITM_TCR_ITMENA; /* ITM Trace Control Register */
    ITM_TPR = 0xF; /* ITM Trace Privilege Register */
    ITM_TER[0] = portBits; /* ITM Trace Enable Register. Enabled tracing on stimulus ports. One bit per stimulus port. */
    *((volatile unsigned *)(ITM_BASE + 0x01000)) = 0x400003FE; /* DWT_CTRL */
    *((volatile unsigned *)(ITM_BASE + 0x40304)) = 0x00000100; /* Formatter and Flush Control Register */
}

#endif




#endif
