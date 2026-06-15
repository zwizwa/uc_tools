/* The point of this thing is to provide an upload and command tool
   for i686/kernel.c based on idempotent commands.  For memory write,
   kernel just responds to packets, sends one ack per packet.  We keep
   going until we got an ack per block back.  This avoids tftp-style
   round-trip delays and keeps the implementation on the target very
   simple. */


/* This uses a generalization of the 3if protocol
   - One UDP message can contain a stream of commands
   - Operation can be asynchronous
   - Separate transport header with sequence number, which can be reused after rx ack
   - Should probably limit to idempotent commands only
   - 1-1 relation between UDP packets, but can contain multiple commands
   - Don't rely on 3if machine state between packets (drops)
*/

#include "macros.h"

#include "tcp_tools.h"
#include "mmap_file.h"

/* Try to get everything integrated here. */
#define TETHER_3IF_UDP // Not enabled by default
#include "mod_tether_3if.c"

void tether_handle_async(struct tether *s) {
    ERROR("tether_udp: async not implemented\n");
}

#include <poll.h>


void usage_exit(int argc, char **argv) {
    LOG("usage: %s with arguments:\n");
    LOG("  send    <host> <file>\n",   argv[0]);
    exit(1);
}

#if 0
void tether_udp_transact(struct tether_udp *s,
                         struct mon_packet *c) {
    c->sequence_number = s->sequence_number++;
    tether_udp_send_packet(s, (void*)c, c->data_len);

    struct sockaddr_in from;
    socklen_t fromlen = sizeof(from);
    int flags = 0;

    struct pollfd pfd[] = {
        [0] = { .events = POLLIN, .fd = s->fd },
    };
    int rv;
    ASSERT_ERRNO(rv = poll(&pfd[0], ARRAY_SIZE(pfd), -1));
    ASSERT(rv >= 0);
    if (rv == 0) {
        LOG("timeout\n");
        exit(1);
    }
    ASSERT(pfd[0].revents & POLLIN);
    struct mon_packet r = {};

    rv = recvfrom(s->fd, &r, 1+sizeof(r.data), flags,
                  (struct sockaddr *)&from, &fromlen);
    // Note that 0 length is legal UDP but not part of 3if
    // protocol.  There will always be one sequence byte.
    ASSERT(rv >= 1);
    LOG("len %d seq=0x%02d\n", rv, r.sequence_number);

}
#endif

void cmd_send(int argc, char **argv) {
    if (argc < 3) { usage_exit(argc, argv); }
    const char *host    = argv[2];
    const char *file    = argv[3];
    struct tether s;
    tether_open_udp(&s, host, 799); // 0x31f
    s.verbose  = 2;
    s.progress = 1;

    struct mmap_file mf = {};
    mmap_file_open_ro(&mf, file);
    LOG("%s: %d bytes\n", file, mf.size);

    tether_iub_write(&s, 0x90000, mf.buf, mf.size);
}


int main(int argc, char **argv) {
    if (argc < 2)                         usage_exit(argc, argv);
    else if (!strcmp(argv[1], "send"))    cmd_send(argc, argv);
    else                                  usage_exit(argc, argv);
}

