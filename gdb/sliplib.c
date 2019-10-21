
#include "sliplib.h"

/* Convert SLIP input to an abstract byte/end event stream. */
void slip_write_tagged(struct slip_write_state *s, const uint8_t *buf, uint32_t len) {
    for(uint32_t i = 0; i < len; i++) {
        uint8_t byte = buf[i];
        if (SLIP_ESC == s->last) {
            if (SLIP_ESC_END == byte) {
                s->byte(s, SLIP_END);
            }
            else if (SLIP_ESC_ESC == byte) {
                s->byte(s, SLIP_ESC);
            }
            else {
                // Just write the byte.  We can't handle errors anyway.
                s->byte(s, byte);
            }
        }
        else if (SLIP_ESC == byte) {
            // Nothing to do here
        }
        else if (SLIP_END == byte) {
            // Packet is done, call the handler
            s->end(s);
        }
        else {
            s->byte(s, byte);
        }
        s->last = byte;
    }
}

/* Convert a byte stream to double-ended slip, wasting as little
 * memory as possible.  Write only complete messages, hence a minimum
 * of 6 bytes is necessary for this to do anything. */
uint32_t slip_read_tagged(
    uint16_t tag,
    uint32_t (*read)(uint8_t *buf, uint32_t len),
    uint8_t *buf, uint32_t room) {

    /* We need at least room for END TAG ESC x END */
    if (room < 6) return 0;
    uint8_t byte = 0;
    /* Don't generate a frame if there is no data. */
    if (1 != read(&byte, 1)) return 0;

    uint32_t i = 0;
    buf[i++] = SLIP_END;
    buf[i++] = tag >> 8;
    buf[i++] = tag & 0xFF;
    for(;;) {
        /* Precondition: There is room for at least one escaped
         * character and an END byte. */
        if (SLIP_END == byte) {
            buf[i++] = SLIP_ESC;
            buf[i++] = SLIP_ESC_END;
        } else if (SLIP_ESC == byte) {
            buf[i++] = SLIP_ESC;
            buf[i++] = SLIP_ESC_ESC;
        }
        else {
            buf[i++] = byte;
        }
        /* Ensure precondition of next iteration holds before
         * attempting to read the next byte. */
        if ((room-i) < 3) break;
        if (1 != read(&byte, 1)) break;
    }
    buf[i++] = SLIP_END;
    return i;
}
