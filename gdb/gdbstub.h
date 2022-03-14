#ifndef GDBSTUB_H
#define GDBSTUB_H

//#include "base.h"

#include <stdint.h>
#include "rsp_packet.h"

// Currently fixed, size constrained by commands: req: X, rpl: g
#define GDBSTUB_PACKET_BUFFER_SIZE 220

#define GDBSTUB_PACKET(name)                                            \
    static uint8_t name##_buf[GDBSTUB_PACKET_BUFFER_SIZE];              \
    static struct packet name = {.buf = name##_buf, .size = sizeof(name##_buf)}
#define GDBSTUB_PACKET_NODATA(name) \
    struct packet name = {}

#define GDBSTUB_INSTANCE(stub, cmds)                           \
    GDBSTUB_PACKET(stub##_req);                                \
    GDBSTUB_PACKET(stub##_rpl);                                \
    struct gdbstub stub = {                                    \
        .req = &stub##_req,                                    \
        .rpl = &stub##_rpl,                                    \
        .reg = GDBSTUB_REG_INIT,                               \
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
#define GDBSTUB_REG_INIT {[13] = 0x20005000}
#define GDBSTUB_NB_REGS 26
#define GDBSTUB_FLAG_STARTED (1 << 0)
#define GDBSTUB_FLAG_LOOP    (1 << 1)
struct gdbstub {
    uint32_t flags;
    struct packet *req;
    struct packet *rpl;
    uint32_t reg[GDBSTUB_NB_REGS];
    uint32_t breakpoint;
    const struct gdbstub_command *commands;
};
#define GDBSTUB_FLAG_MEMORY_PROTECT (1<<0)





// RSP state machine i/o
void     gdbstub_write(struct gdbstub *stub, const uint8_t *inbuf, uint32_t size);
uint32_t gdbstub_read (struct gdbstub *stub, uint8_t *buf, uint32_t size);
uint32_t gdbstub_read_ready(struct gdbstub *stub);

// Provided by target-specific code.
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
