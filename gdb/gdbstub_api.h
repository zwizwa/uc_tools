#ifndef GDBSTUB_API_H
#define GDBSTUB_API_H

/* This interface file is free and unencumbered software released into
   the public domain.

   Anyone is free to copy, modify, publish, use, compile, sell, or
   distribute this software, either in source code form or as a
   compiled binary, for any purpose, commercial or non-commercial, and
   by any means.

   In jurisdictions that recognize copyright laws, the author or
   authors of this software dedicate any and all copyright interest in
   the software to the public domain. We make this dedication for the
   benefit of the public at large and to the detriment of our heirs
   and successors. We intend this dedication to be an overt act of
   relinquishment in perpetuity of all present and future rights to
   this software under copyright law.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
   CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   For more information, please refer to <http://unlicense.org/>
*/

/* For use with gdbstub.c

   Bootloader, host and stub communicate using two pieces of memory:

   - 256 bytes of RAM shared between loader and app.
     This exposes the Bootloader's USB main loop to the app.

   - 1024 bytes of Flash (one erase unit) for app.

     This exposes app's binary API to host, allowing the host to
     invoke app code through the GDB RSP without needing the .elf (for
     symbols) or a special purpose protocol.

   For stm32f103 (see stm32f103.ld):
     RAM:   0x20000000
     Flash: 0x08002800
 */

#include "gdbstub.h"

typedef void (*gdbstub_fn_poll)(void);
typedef void (*gdbstub_fn_reset)(void);
typedef void (*gdbstub_fn_add)(gdbstub_fn_poll);
typedef uint32_t (*gdbstub_fn_read)(uint8_t*, uint32_t);
typedef void (*gdbstub_fn_write)(const uint8_t*, uint32_t);

typedef void (*gdbstub_fn_start)(void);
typedef void (*gdbstub_fn_stop)(void);

typedef void (*gdbstub_fn_loop)(gdbstub_fn_poll);

struct gdbstub_io {
    gdbstub_fn_read read;
    gdbstub_fn_write write;
};

struct gdbstub_service {
    gdbstub_fn_add             add;        // add a poll fuction
    gdbstub_fn_reset           reset;      // reset all callbacks
    struct gdbstub_io          rsp_io;     // access GDB RSP state machine (1)
    struct gdbstub_io const ** io;         // current serial port connection (2)
    struct gdbstub*            stub;       // bootloader's GDB RSP stub state
    void*                      stack_lo;   // low and high address of stack space
    void*                      stack_hi;
};
/* Instance is stored in Flash - see stm32f1.ld */
extern const struct gdbstub_service _service;

/* There is room for a fixed number of poll tasks.  If more are
   needed, the firmware should provide its own mechanism. */
#define GDBSTUB_SERVICE_NB_POLL 16

/* Regarding protocol:
   (1) Gives access to the debugger stub using the GDB RSP protocol
   (2) Interprets read/write on the CDC ACM serial port (application protocol)
   This allows:
   - Switching protocols through _service.switch_protocol
   - Encapsulating GDB RSP in the application protocol.
*/



/* This reflects core.ld, app.ld
   First partition up to _CONF is bootloader code.
   Second partition between _CONF and _APP is bootloader config (0x800 == minimum erase size).
*/
#define GDBSTUB_ADDR_CONF 0x08002800
#define GDBSTUB_ADDR_APP  0x08003000


/* Basic firmware architecture:

   - Applications run as extensions to bootloader, implemented as gdbstub.

   - The bootloader comes up by default, allowing firmware upgrade
     using a standard PC+GDB combo.

   - If bootloader receives a packet it doesn't understand, it calls
     the switch_protocol() function defined by the application, which can
     implement an arbitrary protocol on the USB ACM virtual serial
     port.  This allows the use of a more efficient binary protocol.


   Interface:

   - start/stop: Intended to enable/disable background tasks.  When
     stopped, application code can be replaced.  Applications are not
     started automatically.

   - switch_protocol: Switch i/o protocol from GDB RSP to an arbitrary
     application-defined protocol, on reception of a packet that is
     not understood.

   - USB strings: override the default USB strings.

   - modtype: used by GateWay code to distinguish application type.

   - Memory protection control: Disallow access to bootloader code
     through Flash erase/write commands to avoid "bricking" a
     bootloader that is embedded in an assembly.

   - monitor: GDB "monitor" command dispatch to application.  This can
     be used to access firmware functionality from a simple PC+GDB
     setup, i.e. without the need for GateWay device.

*/


/* AUTOSTART

   DO NOT call start() automatically on a board that does not have:
   - an accessible JTAG connector
   - a hardware override that prevents start() to be called

   If this rule of thumb is violated it is possible to brick the
   device, meaning it boots into corrupt code without there being a
   way to prevent it.

   For this reason, the gdbstub code does not provide autostart
   functionality.  Implement this on a case-by-case basis for each
   board's bootloader.

*/

#define GDBSTUB_CONFIG_USB_ISR (1 << 0)

struct info_buf;
struct swd_tether;
struct gdbstub_control;

struct cmd_3if;
typedef void (*cmd_3if)(struct cmd_3if *);

struct gdbstub_config {

    /*  0: USB strings */
    const char *manufacturer;
    const char *product;
    const char *serial;

    /*  3: Application control */
    gdbstub_fn_start start;
    gdbstub_fn_stop  stop;

    /*  5: Memory protection config. */
    uint32_t bottom;

    /*  6: gdbstub flags initial setting. */
    uint32_t flags_default;

    /*  7: nvram location */
    uint32_t nvram;

    /*  8: Firmware tag (e.g. filename). */
    const char* firmware;

    /*  9: Firmware build date + version. */
    const char* version;

    /* 10: GDB monitor command handler. */
    const char* (*monitor)(const char *);

    /* 11: Connect application console. */
    void (*switch_protocol)(const uint8_t*, uint32_t size);

    /* 12: Main loop takeover function. */
    gdbstub_fn_loop loop;

    /* 13: Protocol. */
    const char *protocol;

    /* 14: Protocol to host, if different from protocol.  This allows
     * e.g. a custom binary command protocol coming from host, but
     * plain log messages or formatted text going back to host. */
    const char *protocol2;

    /* 15, 16: Firmware address range in Flash.  This is not needed by
       the gdbstub loader, but makes a the raw binary image
       self-contained in case of secondary loaders.  E.g. see
       trampoline.c */
    const uint8_t *flash_start;
    const uint8_t *flash_endx;

    /* 17: Log buffer, for external debugger access.  See info_buf.h */
    struct info_buf_hdr *info_buf;

    /* 18: Firmware control block. */
    struct gdbstub_control *control;

    /* 19: Optional tag or flags for code handling firmware images. */
    uint32_t fwtag;

    /* 20: This slot can be used to point to any othere data in the
       partition.  Note that the fields flash_start and flash_endx
       must be zero such that partition_config_valid() returns NULL in
       case there is no bootable code in this partition apart from
       data pointed to here. */
    void *data;

    /* 21: Alternative to swiching protocol: app presents read/write. */
    const struct gdbstub_io *io;

    /* 22: Interpreter extensions for 3if.  It is assumed that
       firmware and tether application know the layout of this
       array. */
    const cmd_3if *cmd_3if;

    /* 23: Reserved */
    void *reserved_23[32-23];
};

/* Note that gstub_config can only be used directly on a 32-bit
   platform where uintptr_t is uint32_t.  For access on build host,
   cast the struct to uint32_t array and use these indices. */

/* Indices for uint32_t array overlayed on top of config block.  For
   use in preparing binary firmware images.  See bin2fw.c */
#define GDBSTUB_CONFIG_INDEX_VERSION      9
#define GDBSTUB_CONFIG_INDEX_FLASH_START 15
#define GDBSTUB_CONFIG_INDEX_FLASH_ENDX  16
#define GDBSTUB_CONFIG_INDEX_CONTROL     18
#define GDBSTUB_CONFIG_INDEX_FWTAG       19
#define GDBSTUB_CONFIG_INDEX_CMD_3IF     22

extern struct gdbstub_config _config; // FLASH


/* Control block, appended to the firmware image after compilation.
   Currently only contains CRC, but later could contain information
   that influences the boot process.  Note that this is not used by
   the gdbstub loader, but it is useful to standardize it here.  See
   e.g. trampoline.c */
struct gdbstub_control {
    uint32_t size;         /* sizeof(struct gdbstub_control) */
    uint32_t version;      /* Control block version/magic.  Currently 0, not used. */
    uint32_t fw_crc;       /* CRC of firmware image. */
    uint32_t priority;     /* Load priority.  0=lowest. */
    uint8_t  fw_sha1[20];  /* SHA-1 hash of firmware, see bin2fw.c */
    uint32_t ctrl_crc;     /* This should always be the last field. */
} __attribute__ ((__packed__));

/* To stop an application, disable all interrupts and call this function. */
static inline void gdbstub_service_stop(const struct gdbstub_service *s) {
    s->reset();
    *(s->io) = &(s->rsp_io);
    s->stub->ctrl->flags &= (~GDBSTUB_FLAG_STARTED);
}

/* Non-volatile configuration header and data go in a separate linker
   section to ensure they are placed at the correct Flash offset. */
#define CONFIG_HEADER_SECTION __attribute__ ((section (".config_header")))
#define CONFIG_DATA_SECTION   __attribute__ ((section (".config_data")))

/* Service table goes after the bootloader's vector table. */
#define SERVICE_SECTION       __attribute__ ((section (".service")))

/* End of uC program Flash is used to store data, e.g. FPGA image. */
#define STORE_HEADER_SECTION  __attribute__ ((section (".store_header")))
#define STORE_DATA_SECTION    __attribute__ ((section (".store_data")))

/* Firmware control block contains things like CRC, which is computed
   after linking has finished and patched into the elf. */
#define CONTROL_SECTION       __attribute__ ((section (".control")))

/* Can be generated by gdb/build.sh */
extern const char config_version[];



#endif
