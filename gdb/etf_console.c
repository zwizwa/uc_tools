/* Wrapper for libprim-based command line interpreters.

   The problem this solves is that the libprim code requires blocking
   I/O.  Which can be implemented on the serial port, but currently
   not on USB.  The intermediate route is to use ETF to perform the
   chunking necessary to send command buffers, and run the interpreter
   on the command buffer. */

#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include <stdlib.h>

// libprim
#include "leaf/port.h"
#include "leaf/bytes.h"


struct sm_etf sm_etf;
uint8_t etf_buf[1024];

// For malloc.
extern int _ebss;
void *_sbrk(intptr_t increment) {
    static uint8_t *brk = (void*)&_ebss;
    brk += increment;
    infof("_sbrk %x %x\n", increment, brk);
    return brk;
}
// FIXME: These should not be necessary.  Find out what code is calling this.
void _exit(void)  { info_puts("_exit\n");   while(1); }
int _isatty(void) { info_puts("_isatty\n"); return 0; }
int _read(void)   { info_puts("_read\n");   return -1; }
int _write(void)  { info_puts("_write\n");  return -1; }
int _close(void)  { info_puts("_close\n");  return -1; }
int _fini(void)   { info_puts("_fini\n");   return -1; }
int _fstat(void)  { info_puts("_fstat\n");  return -1; }
int _lseek(void)  { info_puts("_lseek\n");  return -1; }


uint32_t interpret(uint8_t *buf, uint32_t len) {
    //bytes *b = bytes_const_new((const char*)buf, len);
    //port *p  = port_bytes_new(b);
    //infof("brk: %x\n", _sbrk(0));
    void *p = malloc(10);
    infof("%x\n", p);
    free(p);
    //leaf_free(&p->base);
    //leaf_free(&b->base);
    return 0;
}




// Run the interpreter on a binary supplied using ETF.
static uint32_t cb(struct sm_etf *sm) {
    if (sm->data_type != BINARY_EXT) return 0x100;
    if (sm->depth != 0) return 0x101;
    return interpret(sm->buf, sm->data_size);
    //info_write(sm->buf, sm->data_size);
    //infof("%x\n", &_ebss);
    return 0;
}
static void sm_etf_reset(void) {
    sm_etf_init(&sm_etf, &etf_buf[0], sizeof(etf_buf), &cb);
}
static void etf_write(const uint8_t *buf, uint32_t len) {
    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    if (SM_WAITING != status) {
        infof("status=%x\n",status);
        sm_etf_reset();
    }
}
static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    //return etf_binary_read(info_read, buf, len);
    return etf_tagged_read(123, info_read, buf, len);
}
const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};



// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init.  Note that this performs memory
     * initialization that would normally happen before main() is
     * called. */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    sm_etf_reset();

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "ETF Test";
const char config_serial[]       CONFIG_DATA_SECTION = "0";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};



