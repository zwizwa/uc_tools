#ifndef MOD_PROTOCOL_C
#define MOD_PROTOCOL_C

#ifndef SLIP_OUT_SPILL_SIZE
#define SLIP_OUT_SPILL_SIZE 40
#endif

/* FIXME: This should probably go in a library of common protocol routines. */
#define SLIP_OUT_FROM(hdr,read) \
    cbuf_write_slip_from_read(\
        &slip_out, \
        &hdr[0], sizeof(hdr), \
        SLIP_OUT_SPILL_SIZE, \
        read)
const uint8_t hdr_info[]    = {U16_BE(TAG_INFO)};
const uint8_t hdr_gdb[]     = {U16_BE(TAG_GDB)};
const uint8_t hdr_plugio[]  = {U16_BE(TAG_PLUGIO)};
const uint8_t hdr_stream0[] = {U16_BE(TAG_STREAM), U16_BE(0)};


#endif
