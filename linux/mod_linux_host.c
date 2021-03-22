/* Run an Arduino-style setup/loop/handle_tag_u32 C module inside a
   Linux process.  */

#include "packet_bridge.h"
#include "uct_byteswap.h"
#include "tag_u32.h"
#include <string.h>

#ifndef MOD_LINUX_HOST_C
#define MOD_LINUX_HOST_C

void loop(void);
void setup(void);

int handle_tag_u32(struct tag_u32*);

struct port *port;
#define SEND_TAG_U32_BUF_WRITE(...) port->write(port, __VA_ARGS__)

#include <stdarg.h>
#include "mod_send_tag_u32.c"

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
                tag_u32_dispatch(
                    handle_tag_u32,
                    send_reply_tag_u32,
                    NULL, buf, rv);
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
