/* Non-blocking GDB RSP stub for event-driven application.

   Tested on STM32F103, but adaptation to other Cortex M should be
   straightforward.

   This is not a full GDB stub (yet).  Main features:
   - event driven i/o to run it from libopencm3 USB callbacks
   - memory inspection
   - code execution through gdb's "print" and "call" commands

   This code used Tomasz Malesinski's arm-stub.c in the Rockbox
   project as a starting point, but has been modified beyond
   recognition:

   - Replace loops with state machines
   - Smaller I/O buffers
   - Removed use of scanf, replaced by memcpy and manual hex read
   - Added hack for gdb "call" command
   - Added state machine main loop
   - Use "chained ||" error checking.
   - Add test stub
   - Integrate with libopencm3

   Remarks:

   - Switching between several stubs in one gdb session seems to cause
     problems.  Best to launch one gdb session per stub.

   [ts]
*/

#include "generic.h"

#define _BSD_SOURCE
#include <stdint.h>
#include <string.h>
#include <stdio.h>

#include "gdbstub.h"
#include "gdbstub_api.h"



// Input / output packets and control state.  The req packet contains
// the unpacked format (only RSP payload), while the rpl packet
// contains the escaped packet in "+$...#xx" form.


// Limits are set by PacketSize= in qSupported reply.

static inline int started(struct gdbstub *stub) {
    return !!(stub->flags & GDBSTUB_FLAG_STARTED);
}
static inline void set_started(struct gdbstub *stub, int started) {
    if (started) {
        stub->flags |= GDBSTUB_FLAG_STARTED;
    }
    else {
        stub->flags &= ~GDBSTUB_FLAG_STARTED;
    }
}

static inline void ensure_started(struct gdbstub *stub) {
    if (started(stub)) return; // idempotent
    _config.start();
    set_started(stub, 1);
}
static inline void ensure_stopped(struct gdbstub *stub) {
    if (!started(stub)) return; // idempotent
    _config.stop();
    set_started(stub, 0);
}
static int32_t mon_start_stop(struct gdbstub *stub, uint32_t start) {
    uint32_t was = started(stub);
    if (was == start) {
        // Already in desired state
    }
    else {
        gdbstub_fn_start fn = start ? _config.start : _config.stop;
        if (fn) {
            fn();
            set_started(stub, 1);
        }
    }
    const char reply[] = {
        '0'+ was,'-','>','0'+ started(stub),'\n',0
    };
    return rsp_hex_cstring(stub->rpl, reply);
}


// Monitor commands and replies are sent hex-encoded.
static int32_t cmd_Rcmd(struct gdbstub *stub, const uint8_t *cmd, uint32_t cmd_len) {
    uint32_t rc_len = cmd_len / 2;
    uint8_t rc[rc_len+1];
    for (uint32_t i = 0; i<rc_len; i++) {
        rc[i] = read_hex_byte(cmd + 2*i);
    }
    rc[rc_len] = 0;
    LOG("Rcmd: %s\n", rc);

    if (!memcmp("start", (char*)rc, 6)) { return mon_start_stop(stub, 1); }
    if (!memcmp("stop",  (char*)rc, 5)) { return mon_start_stop(stub, 0); }
    if (!flash_null(_config.monitor)) {
        ensure_started(stub);
        const char *reply = _config.monitor((const char*)rc);
        if (!reply) goto error;
        uint32_t max_len = (stub->rpl->size - 5)/2; // '+$.....#XX
        if (strlen(reply) > max_len) goto error;
        return rsp_hex_cstring(stub->rpl, reply);
    }
  error:
    return rsp_hex_cstring(stub->rpl, "?\n");
}


  /* - RAM and Flash should be accurate to avoid bad accesses.

     - Create one large block for all peripherals and system
       registers.  Do not include the addresses near the end of the
       image as they are most likely just due to bad memory / stack
       pointers. */

extern const char gdbstub_memory_map[];

static int32_t cmd_Supported(struct gdbstub *stub,
                             const uint8_t *cmd, uint32_t cmd_len) {
    return rsp_begin(stub->rpl)
        || packet_save_string_cs(stub->rpl,
                                 "qXfer:memory-map:read+;"
                                 "PacketSize=c0")
        || rsp_end(stub->rpl);
}

static int32_t cmd_xfer_memory_map_read(struct gdbstub *stub,
                                        const uint8_t *b, uint32_t l) {
    uint32_t offset, len;
    TRY(take_hex(&b, &l, &offset) ||
        take_hex(&b, &l, &len));
    uint32_t mm_size = strlen(gdbstub_memory_map);
    if (offset > mm_size) len = 0;
    else if (offset + len > mm_size) len = mm_size - offset;

    uint8_t tag = (offset + len >= mm_size) ? 'l' : 'm'; // last or more

    return rsp_begin(stub->rpl)
        || packet_save_cs(stub->rpl, tag)
        || packet_save_nstring_cs(stub->rpl, gdbstub_memory_map + offset, len)
        || rsp_end(stub->rpl);
}

#define CHECK_ADDR(rpl, addr)                                            \
    if (!flash_null((void*)(intptr_t)_config.bottom) && (addr < _config.bottom)) { \
        return rsp_E(rpl, 00);                                      \
    }

static int32_t cmd_flash_erase(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t addr, size;
    TRY(take_hex(&b, &l, &addr) ||
        take_hex(&b, &l, &size));
    CHECK_ADDR(stub->rpl, addr);
    flash_erase(addr, size);
    return rsp_OK(stub->rpl);
}
static int32_t cmd_flash_write(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t addr;
    TRY(take_hex(&b, &l, &addr));
    CHECK_ADDR(stub->rpl, addr);
    flash_write(addr, b, l);
    return rsp_OK(stub->rpl);
}
static int32_t cmd_flash_done(struct gdbstub *stub, const uint8_t *cmd, uint32_t cmd_len) {
    // Currently not needed.  STM32 can write a halfword at a time.
    return rsp_OK(stub->rpl);
}

static int32_t cmd_signal(struct gdbstub *stub, const uint8_t *cmd, uint32_t n) {
    return rsp_begin(stub->rpl)
        || packet_save_cs(stub->rpl, 'S')
        || packet_save_hex_cs(stub->rpl, 5)  // SIGTRAP
        || rsp_end(stub->rpl);
}

/* The stub doesn't support generic breakpoints (yet), but allows
   execution of remote code as part of gdb's "print" or "call"
   commands.

   FIXME: Starting argument 5, arguments will go on the current stack
   pointed to by reg[13].
*/


typedef uint32_t (*gen_func)(uint32_t a, uint32_t b, uint32_t c, uint32_t d);
static int32_t cmd_continue(struct gdbstub *stub, const uint8_t *cmd, uint32_t n) {
    if (stub->breakpoint) {
        gen_func fn = (gen_func)(unsigned long)(stub->reg[15]|1);
        stub->reg[0] = fn(stub->reg[0],stub->reg[1],stub->reg[2],stub->reg[3]);
        // Fake stop at breakpoint
        stub->reg[15] = stub->breakpoint + 1;
    }
    return cmd_signal(stub, cmd, n);
}


static int32_t cmd_get_registers(struct gdbstub *stub, const uint8_t *cmd, uint32_t n) {
    rsp_begin(stub->rpl);
    for (uint32_t i = 0; i < GDBSTUB_NB_REGS; i++) {
        int32_t rv = packet_save_u32_hex_cs(stub->rpl, stub->reg[i]);
        if (rv) return rv;
    }
    return rsp_end(stub->rpl);
}

static int32_t cmd_get_register(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t r;
    TRY(take_hex(&b, &l, &r));
    if (r >= GDBSTUB_NB_REGS) return E_PROTO;
    return rsp_begin(stub->rpl)
        || packet_save_u32_hex_cs(stub->rpl, stub->reg[r])
        || rsp_end(stub->rpl);
}

static int32_t cmd_set_registers(struct gdbstub *stub, const uint8_t *cmd, uint32_t n) {
    if (n != 26*8) return E_PROTO;
    uint32_t nr = n / 8;
    for (uint32_t i = 0; i < nr; i++) {
        stub->reg[i] = read_hex_u32_le(cmd + i * 8);
    }
    return rsp_OK(stub->rpl);
}

static int32_t cmd_set_register(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t r;
    TRY(take_hex(&b, &l, &r));
    if (r >= GDBSTUB_NB_REGS) return E_PROTO;
    stub->reg[r] = read_hex_u32_le(b);
    return rsp_OK(stub->rpl);
}

static int32_t cmd_get_memory(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t addr, len;
    TRY(take_hex(&b, &l, &addr) ||
        take_hex(&b, &l, &len));
    //LOG("addr=%x, len=%x\n", addr, len);
    rsp_begin(stub->rpl);
    for (uint32_t i = 0; i < len; i++) {
        uint8_t v = mem_read(addr+i);
        int32_t rv = packet_save_hex_cs(stub->rpl, v);
        if (rv) return rv;
    }
    return rsp_end(stub->rpl);
}



/* This is command 'M'.

   Notes:

   - command 'X' isn't necessary. GDB will probe for it, and will use
     'M' (the one above) instead.  'M' was removed after git
     2e697e80cc341876c6ed844edfaffd551378e2dc

   - 32-bit writes are performed when addresses are 32-bit aligned.
     This is necessary for some registers, e.g. AIRCR.
*/

static int32_t cmd_set_memory(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t addr, len;
    TRY(take_hex(&b, &l, &addr) ||
        take_hex(&b, &l, &len));
    LOG("addr=%x, len=%x\n", addr, len);

    uint32_t endx = addr + len;

    while(addr < endx) {
        int32_t rv;
        /* Perform 32 byte write if possible. */
        if (((addr & 3)    == 0) &&  // this is a word boundary, and
            ((endx - addr) >= 4)) {  // at least 1 word to write
            rv = mem_write32(addr, read_hex_u32_le(b));
            addr += 4;
            b    += 8;
        }
        else {
            rv = mem_write(addr, read_hex_byte(b));
            addr += 1;
            b    += 2;
        }
        if (rv) return rv;
    }
    return rsp_OK(stub->rpl);
}


static int32_t cmd_set_breakpoint(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t bp_num, addr, kind;
    TRY(take_hex(&b, &l, &bp_num) ||
        take_hex(&b, &l, &addr)   ||
        take_hex(&b, &l, &kind));
    // FIXME: bp_num is ignored
    stub->breakpoint = addr;
    return rsp_OK(stub->rpl);
}
static int32_t cmd_remove_breakpoint(struct gdbstub *stub, const uint8_t *b, uint32_t l) {
    uint32_t bp_num, addr, kind;
    TRY(take_hex(&b, &l, &bp_num) ||
        take_hex(&b, &l, &addr)   ||
        take_hex(&b, &l, &kind));
    // FIXME: bp_num is ignored
    stub->breakpoint = 0;
    return rsp_OK(stub->rpl);
}

/* Command interpreter */

#define CMD(tag, fn) {(uint8_t*)(tag),fn}
const struct gdbstub_command gdbstub_default_commands[] = {

    CMD("m", cmd_get_memory),
    CMD("?", cmd_signal),
    CMD("c", cmd_continue),
    CMD("g", cmd_get_registers),
    CMD("G", cmd_set_registers),
    CMD("p", cmd_get_register),
    CMD("P", cmd_set_register),
    CMD("M", cmd_set_memory),
    CMD("Z", cmd_set_breakpoint),
    CMD("z", cmd_remove_breakpoint),

    CMD("qRcmd,", cmd_Rcmd),
    CMD("qSupported:", cmd_Supported),
    CMD("qXfer:memory-map:read::", cmd_xfer_memory_map_read),

    CMD("vFlashErase:", cmd_flash_erase),
    CMD("vFlashWrite:", cmd_flash_write),
    CMD("vFlashDone",   cmd_flash_done),

    {}
};



static int32_t rsp_interpret(struct gdbstub *stub, const uint8_t *cmd, uint32_t cmd_len) {
    for (const struct gdbstub_command *r = stub->commands; r->tag; r++) {
        uint32_t tl = strlen((const char*)r->tag);
        // LOG("cmd=%s, tag=%s, tl = %d\n", cmd, r->tag, tl);
        if ((cmd_len >= tl) && (!memcmp(cmd, r->tag, tl))) {
            return r->fn(stub, cmd + tl, cmd_len - tl);
        }
    }
    // Return empty packet for unsupported commands.
    return rsp_begin(stub->rpl)
        || rsp_end(stub->rpl);
}
static uint32_t rsp_check_interpret(struct gdbstub *stub) {
    packet_init(stub->rpl);
    if (stub->req->expected != stub->req->checksum) {
        /* bad reception, send nack */
        LOG("check %02x %02x", stub->req->expected, stub->req->checksum);
        return packet_save(stub->rpl, '-');
    }
    else {
        /* always ack on good reception even if packet is not
         * understood, in which case an empty reply is sent. */
        // LOG("%s\n", req_buf); LOG("\n");
        return rsp_interpret(stub, stub->req->buf, stub->req->wr);
    }
}
void gdbstub_interpret(struct gdbstub *stub) {
    /* Packet is complete */
    int rv = rsp_check_interpret(stub);
    if (rv < 0) {
        /* GDB error reply. */
        packet_init(stub->req);
        packet_init(stub->rpl);
        rsp_E(stub->rpl, rv);
    }
    else {
        /* Interpretation OK. */
    }
}



/* I/O buffer event processor
   Call these when input/output buffers are ready for with/for more data. */
void gdbstub_write(struct gdbstub *stub, const uint8_t *inbuf, uint32_t size) {
    /* If a request is received before a reply transfer is complete,
       something went wrong and the output transfer needs to be
       aborted. */
    packet_init(stub->rpl);

    /* Push the entire input in.  If this contains more than one
       request, all but the last reply is ignored. */
    for(uint32_t i=0; i<size; i++) {
        int rv = rsp_decode_putchar(stub->req, inbuf[i]);
        if (rv == E_BUSY) continue;

        if (rv == E_OK) {
            gdbstub_interpret(stub);
            /* Reply is in rpl and will be picked up by USB IN
               callback */
        }
        else if (rv == E_ABORT) {
            /* Protocol error.  We take this to mean that the data
             * was intended for the application.  Start and pass
             * it on. */
            if (!flash_null(_config.switch_protocol)) {
                LOG("starting app io\n");
                ensure_started(stub);
                _config.switch_protocol(inbuf, size);
                return;
            }
        }
        else {
            // NOT REACHED
        }
    }
}

uint32_t gdbstub_read_ready(struct gdbstub *stub) {
    return stub->rpl->wr - stub->rpl->rd;
}

uint32_t gdbstub_read(struct gdbstub *stub, uint8_t *buf, uint32_t size) {
    uint32_t left = gdbstub_read_ready(stub);
    if (!left) return 0;
    uint32_t chunk = size < left ? size : left;
    memcpy(buf, stub->rpl->buf + stub->rpl->rd, chunk);
    stub->rpl->rd += chunk;
    return chunk;
}





