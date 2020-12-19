/* Run an Arduino-style setup/loop/handle_tag_u32 C module inside a
   Linux process.  */

#include "packet_bridge.h"
#include "byteswap.h"
#include "tag_u32.h"
#include <string.h>

#ifndef MOD_LINUX_HOST_C
#define MOD_LINUX_HOST_C

void loop(void);
void setup(void);

int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes);

struct port *port;

/* Keep the interface symmetric. */
void send_tag_u32(
    void *context, /* Why is this here? */
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    uint32_t buf_size = 4 + 4 * nb_args + nb_bytes;
    uint8_t buf[buf_size];
    uint32_t n = 0;
    write_be(buf+n, TAG_U32, 2); n+=2;
    write_be(buf+n, nb_args, 2); n+=2;
    for (uint32_t i=0; i<nb_args; i++) {
        write_be(buf+n, arg[i], 4); n+=4;
    }
    memcpy(buf+n, bytes, nb_bytes); n+=nb_bytes;
    ASSERT(n == buf_size);
    port->write(port, buf, buf_size);
}
#define SEND_TAG_U32(...) {                                     \
        uint32_t a[] = { __VA_ARGS__ };                         \
        send_tag_u32(NULL,a,sizeof(a)/sizeof(uint32_t),NULL,0); \
}


int main(int argc, char **argv) {
    const char *spec = "-:4";
    if (argc >= 2) { spec = argv[1]; }
    ASSERT(port = port_open(spec));
    setup();
    for(;;) {
        uint8_t buf[1024*64];
        int timeout_ms = 1;
        ssize_t rv = packet_next(port, timeout_ms, buf, sizeof(buf));
        if (!rv) {
            /* Timeout occured, which means some time has elapsed
               since we last called loop(), preventing a busy loop. */
            loop();
        }
        else {
            /* Process TAG_U32 protocol. */
            if ((rv >=4 ) && (TAG_U32 == read_be(buf, 2))) {
                tag_u32_dispatch(handle_tag_u32, NULL, buf, rv);
            }
            /* Ignore evertying else. */
            else {
                ERROR("bad protocol\n");
            }
        }
    }
    return 0;
}

#endif
