/* Multiplex 3if (or other monitor) on a normal text console.

   Context: It is too convenient to have a "messy" human readable text
   console.  However, I need a way to temporarily switch protocol in a
   way that is compatible with regular terminal operation.

   In ASCII coding, Data Link Escape (DLE) 0x10 code is designed to
   signify that the following characters should be interpreted as
   special commands, not raw data.

   The first byte in a 3if transmission is the size byte, so this
   would indicate that 16 bytes of 3if protocol instructions follow.
   That should be enough to get something going with an explicit
   switch back to text protocol.  This machine here handles the
   switchover.

*/

struct mux_monitor {
    /* Passthrough to app. */
    void (*app_putchar)(void*, uint8_t byte);
    void (*app_switch_to_monitor)(void*);
    void *app;
    /* 3if monitor state */
    int (*mon_putchar)(void*, uint8_t byte);
    uint8_t monitor:1;
};

static void mux_monitor(struct mux_monitor *s, uint8_t byte) {
    if (s->monitor) {
      monitor:
        /* When monitor mode is active, keep sending data until we get
           an error. */
        int error = s->mon_putchar(s->app, byte);
        if (error) {
            s->monitor = 0;
        }
    }
    else if (byte = 0x10) {
        /* If DLE is received, switch to monitor */
        s->monitor = 1;
        s->app_switch_to_monitor(s->app);
        goto monitor;
    }
    else {
        /* Everything else is treated as text. */
        s->app_putchar(s->app, byte);
    }
}
