/* Basic routines and boilerplate for SLIP-based program connected to
 * the erl_tools gdbstub_hub.erl framework. */

#include "slipstub.h"
#include "gdbstub_api.h"


/* Application needs to define these if they are used. */
extern struct slipstub slipstub;
extern struct slipstub_buffers slipstub_buffers;

static void slipstub_dispatch(void *ctx, const struct pbuf *p) {
    struct slipstub *s = &slipstub;
    if (p->count < 2) return;
    uint16_t tag = read_be(p->buf, 2);
    switch(tag) {
    case TAG_PING:
        //infof("ping:%d\n",p->count-2);
        cbuf_write_slip_tagged(s->slip_out, TAG_REPLY, &p->buf[2], p->count-2);
        break;
    case TAG_GDB:
        // infof("tag_gdb: %d\n", p->count);
        _service.rsp_io.write(&p->buf[2], p->count-2);
        break;
    default:
        //infof("dispatch %d\n", tag);
        s->dispatch(s, tag, p);
        break;
    }
}
static void slipstub_write(const uint8_t *buf, uint32_t len) {
    pbuf_slip_write(
        buf, len,
        slipstub.slip_in,   // intermediate cbuf for slip data
        slipstub.packet_in, // incoming tagged packet
        slipstub_dispatch, NULL);
}

/* This is for machines that produce streams.  Machines that produce
 * SLIP messagges should be asked to write complete messages
 * elsewhere. */
static int slipstub_poll_read(struct cbuf *b, uint16_t tag,
                              uint32_t (*read)(uint8_t *buf, uint32_t len)) {
    uint8_t buf[40]; // What's a good size?  Maybe make it configurable.
    uint32_t n = read(buf, sizeof(buf));
    if (!n) return 0;
    cbuf_write_slip_tagged(b, tag, buf, n);
    return 1;
}
static void slipstub_poll_streams(struct cbuf *b) {
    if (slipstub_poll_read(b, TAG_INFO, info_read)) return;
    if (slipstub_poll_read(b, TAG_GDB, _service.rsp_io.read)) return;
}
static uint32_t slipstub_read(uint8_t *buf, uint32_t room) {
    /* Flush the output buffer that can be filled by the application.
     * This always needs to run to the end before we can send anything
     * else.  Application should perform a single atomic write.  */
    struct cbuf *c = slipstub.slip_out;
    uint32_t nb = cbuf_read(c, buf, room);
    buf += nb;  room -= nb;
    if (!room) return nb;

    /* There is room for more, so poll each of the state machines
     * according to their priority to generate more data, and do
     * another flush up to available space.  Lazily generating
     * messages like this effectively creates backpressure.  It's
     * often a lot more efficient to let machines hold on to state
     * that can produce a new message, than it is to have them dump
     * serialized data into a buffer at an earlier stage. */
    slipstub_poll_streams(c);
    return nb + cbuf_read(c, buf, room);
}

const struct gdbstub_io slipstub_io = {
    .read  = slipstub_read,
    .write = slipstub_write,
};
void slipstub_switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("SLIP on serial port.\n");
    *_service.io = (struct gdbstub_io *)(&slipstub_io);
    (*_service.io)->write(buf, size);
}

/* Define both slipstub and slipstub_buffers structs if you use
 * this. */
void slipstub_init(slipstub_dispatch_t dispatch) {
    CBUF_INIT(slipstub_buffers.cbuf_from_usb);
    CBUF_INIT(slipstub_buffers.cbuf_to_usb);
    PBUF_INIT(slipstub_buffers.pbuf_from_usb);
    slipstub.slip_in = &slipstub_buffers.cbuf_from_usb;
    slipstub.packet_in = &slipstub_buffers.pbuf_from_usb;
    slipstub.slip_out = &slipstub_buffers.cbuf_to_usb;
    slipstub.dispatch = dispatch;
}
