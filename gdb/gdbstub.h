#ifndef GDBSTUB_H
#define GDBSTUB_H

//#include "base.h"

#include <stdint.h>
#include "rsp_packet.h"
#include "gdbstub_ctrl.h"

// Currently fixed, size constrained by commands: req: X, rpl: g
#define GDBSTUB_PACKET_BUFFER_SIZE 512

#define GDBSTUB_PACKET(name)                                            \
    static uint8_t name##_buf[GDBSTUB_PACKET_BUFFER_SIZE];              \
    static struct packet name = {.buf = name##_buf, .size = sizeof(name##_buf)}
#define GDBSTUB_PACKET_NODATA(name) \
    struct packet name = {}

#define GDBSTUB_INSTANCE(stub, cmds)                           \
    GDBSTUB_PACKET(stub##_req);                                \
    GDBSTUB_PACKET(stub##_rpl);                                \
    struct gdbstub_ctrl stub##_ctrl = {                        \
    };                                                         \
    struct gdbstub stub = {                                    \
        .ctrl = &stub##_ctrl,                                  \
        .req = &stub##_req,                                    \
        .rpl = &stub##_rpl,                                    \
        .regs = GDBSTUB_REGS_INIT,                             \
        .commands = cmds,                                      \
    }


struct gdbstub;
typedef int32_t (*gdbstub_command_fn)(struct gdbstub *stub, const uint8_t *cmd, uint32_t n);

struct gdbstub_command {
    const uint8_t *tag;
    gdbstub_command_fn fn;
};
extern const struct gdbstub_command gdbstub_default_commands[];
extern const struct gdbstub_io *io;


// Stack pointer at top of memory for the (fake) current context.  GDB
// will use this stack frame to perform "print" or "call" commands
// that execute target code.
#define GDBSTUB_REGS_INIT {.sp = 0x20005000}

/* see gdb-7.12.1/gdb/regformats/reg-arm.dat

   Note that the floating point registers are 12 bytes
   https://sourceware.org/bugzilla/show_bug.cgi?id=25162
*/

struct regs {
    uint32_t r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10;
    uint32_t fp,ip,sp,lr,pc;
    uint32_t f0[3];
    uint32_t f1[3];
    uint32_t f2[3];
    uint32_t f3[3];
    uint32_t f4[3];
    uint32_t f5[3];
    uint32_t f6[3];
    uint32_t f7[3];
    uint32_t fps;
    uint32_t cpsr;
};

struct gdbstub {
    struct gdbstub_ctrl *ctrl;
    struct packet *req;
    struct packet *rpl;
    struct regs regs;
    uint32_t breakpoint;
    const struct gdbstub_command *commands;
};
#define GDBSTUB_FLAG_MEMORY_PROTECT (1<<0)





// RSP state machine i/o
void     gdbstub_write(struct gdbstub *stub, const uint8_t *inbuf, uint32_t size);
uint32_t gdbstub_read (struct gdbstub *stub, uint8_t *buf, uint32_t size);
uint32_t gdbstub_read_ready(struct gdbstub *stub);

// Provided by target-specific code.
void clear_cache(void);
uint8_t mem_read(uint32_t addr);
int32_t mem_write(uint32_t addr, uint8_t val);
int32_t mem_write32(uint32_t addr, uint32_t val);
int32_t flash_erase(uint32_t addr, uint32_t size);
int32_t flash_write(uint32_t addr, const uint8_t *buf, uint32_t size);

void gdbstub_interpret(struct gdbstub *stub);


// Medium density: page size 1k
#define GDBSTUB_MEMORY_MAP_STM32F103C8 \
"<memory-map>" \
    "<memory type=\"ram\" start=\"0x20000000\" length=\"0x5000\"/>" \
    "<memory type=\"flash\" start=\"0x8000000\" length=\"0x10000\">" \
        "<property name=\"blocksize\">1024</property>" \
    "</memory>" \
    "<memory type=\"ram\" start=\"0x40000000\" length=\"0xBFFFF000\"/>" \
"</memory-map>"

#define GDBSTUB_MEMORY_MAP_STM32F103CB \
"<memory-map>" \
    "<memory type=\"ram\" start=\"0x20000000\" length=\"0x5000\"/>" \
    "<memory type=\"flash\" start=\"0x8000000\" length=\"0x20000\">" \
        "<property name=\"blocksize\">1024</property>" \
    "</memory>" \
    "<memory type=\"ram\" start=\"0x40000000\" length=\"0xBFFFF000\"/>" \
"</memory-map>"


// high-density: page size 2k
#define GDBSTUB_MEMORY_MAP_STM32F103RD \
"<memory-map>" \
    "<memory type=\"ram\" start=\"0x20000000\" length=\"0x10000\"/>" \
    "<memory type=\"flash\" start=\"0x8000000\" length=\"0x60000\">" \
        "<property name=\"blocksize\">2048</property>" \
    "</memory>" \
    "<memory type=\"ram\" start=\"0x40000000\" length=\"0xBFFFF000\"/>" \
"</memory-map>"

// high-density: page size 2k
#define GDBSTUB_MEMORY_MAP_STM32F103VC \
"<memory-map>" \
    "<memory type=\"ram\" start=\"0x20000000\" length=\"0xC000\"/>" \
    "<memory type=\"flash\" start=\"0x8000000\" length=\"0x40000\">" \
        "<property name=\"blocksize\">2048</property>" \
    "</memory>" \
    "<memory type=\"ram\" start=\"0x40000000\" length=\"0xBFFFF000\"/>" \
"</memory-map>"






#endif
