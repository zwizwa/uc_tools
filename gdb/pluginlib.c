/* Alternative Flash and RAM code loading.
   Compared to GDB stub, this:
   - Loads _much_ faster
   - Has a much simpler protocol
   - Handles start/stop tracking for the plugin with host app running
   - Should be straightforward to extend to multiple plugins
   - Adds an I/O pipe
*/

/* Code for handling plugin load & start */
#include "plugin_api.h"
#include "pbuf.h"

#include "memory.h"

/* Note that plugins have undefined state before they are started, so
   we have to keep track of whether they are started or not. */
int plugin_started_ = 0;
struct plugin_service *plugin_service_ = 0;
static struct plugin_service *plugin_service(void) {
    /* Anything programmed during this session gets priority,
       otherwise use what's stored in Flash */
    if (!plugin_service_) plugin_service_ = &_eflash;
    if (plugin_service_->version != PLUGIN_API_VERSION) return NULL;
    return plugin_service_;
}
static struct plugin_service *plugin_started(void) {
    struct plugin_service *s;
    if ((s = plugin_service()) && plugin_started_) return s;
    return NULL;
}
static void plugin_start(void) {
    struct plugin_service *s = plugin_service();
    /* Start method is required. */
    if (s && s->start && ! plugin_started_) {
        s->start();
        plugin_started_ = 1;
    }
}
static void plugin_stop(void) {
    struct plugin_service *s;
    if ((s = plugin_started()) && s->stop) {
        s->stop();
    }
    plugin_started_ = 0;
}

static uint32_t map_addr(uint32_t addr) {
    if (addr >= 0x08000000) {
        // Assume absolute
        return addr;
    }
    else {
        // Assume relative to _eflash.
        uint32_t abs_addr = addr + (uint32_t)(&_eflash);
        // infof("%08x->%08x\n", addr, abs_addr);
        return abs_addr;
    }
}


uint32_t plugin_read(uint8_t *buf, uint32_t len) {
    /* Do not call any code if the plugin has not explicitly been
     * activated. */
    struct plugin_service *s;
    if ((s = plugin_started()) && s->io.read) {
        return plugin_service()->io.read(buf, len);
    }
    return 0;
}

/* Handle all plugin related messages. */

/* Flash is a separate function to allow it to be used in isolation. */
uint32_t flash_handle_message(const uint8_t *buf, uint32_t len) {
    uint16_t tag = read_be(buf, 2);
    switch(tag) {

    // bp4 ! {send_packet,<<16#FFF6:16, 16#08005000:32, 1024:32, 10:32>>}.
    case TAG_FLASH_ERASE: {
        plugin_stop();
        uint32_t addr = map_addr(read_be(buf+2,  4));
        uint32_t size = read_be(buf+6,  4);
        uint32_t log  = read_be(buf+10, 4);
        int rv = hw_flash_erase(addr, size, log);
        //int rv = 0;
        //if (rv) {
            infof("e:%08x:%d:%d:%d\n", addr, size, log, rv);
        //}
        return len;
    }
    // <<16##FFF7:16,Addr:32,Data/binary>>
    // bp4 ! {send_packet,<<16#FFF7:16,16#08005000:32,1,2,3,4>>}.
    case TAG_FLASH_WRITE: {
        plugin_stop();
        uint32_t req_addr = read_be(buf+2, 4);
        const uint8_t *data_buf  = &buf[6];
        uint32_t addr = map_addr(req_addr);
        uint32_t data_len  = len - 6;
        int rv = hw_flash_write(addr, data_buf, data_len);
        //int rv = 0; (void)data_buf;
        //if (rv) {
            infof("w:%08x:%d:%d\n", addr, data_len, rv);
        //}
        return len;
    }
    default:
        return 0;
    }
}

/* Returns 0 when message is not handled, len otherwise.
   Note that errors do mean the message is handled. */
uint32_t plugin_handle_message(const uint8_t *buf, uint32_t len) {

    if (len < 4) goto not_handled;

    uint16_t tag = read_be(buf, 2);
    switch(tag) {

    case TAG_PLUGCTL: {
        uint16_t cmd = read_be(buf+2, 2);
        switch(cmd) {
        case 0:  // START
            // Note that this message won't get to the host if
            // we crash in the function call.
            plugin_start();
            goto handled;

        case 2:  // STOP
            // Note that this message won't get to the host if
            // we crash in the function call.
            plugin_stop();
            goto handled;

        case 1: { // WRITE BLOCK
            /* Need at least one byte. */
            if (len < (12 + 1)) {
                infof("WRITE_BLOCK short packet %d\n", len);
                goto handled;
            }
            void *ram_load_addr = &_ebss;
            void *flash_load_addr = &_eflash;

            plugin_stop();
            uint32_t rel_addr = read_be(buf+4, 4);
            uint32_t block_log_size = read_be(buf+8, 4);
            const uint8_t *data_buf = &buf[12];
            uint32_t data_len = len-12;
            struct plugin_service *plugin;
            if (rel_addr == 0) {
                /* First block contains the plugin header.  Any
                 * other blocks are assumed to be part of the same
                 * plugin and will re-use the previously written
                 * header. */
                plugin = (void*)data_buf;
                infof("plugin->version   = 0x%08x\n", plugin->version);
                infof("plugin->load_addr = 0x%08x\n", plugin->load_addr);
                infof("plugin->endx_addr = 0x%08x\n", plugin->endx_addr);
                infof("plugin->start     = 0x%08x\n", plugin->start);
                infof("plugin->stop      = 0x%08x\n", plugin->stop);

                /* Keep a copy of the pointer so we can read it out on the
                   next iteration. */
                if (plugin->version == PLUGIN_API_VERSION) {
                    plugin_service_ = plugin->load_addr;
                }
                else {
                    goto bad_plugin;
                }
            }
            else {
                /* On subsequent block, the plugin header will be in
                   Flash. */
                plugin = plugin_service();
                if (!plugin) goto bad_plugin;
            }

            /* We're going to constrain loading to where we expect
             * data to be going. */
            void *abs_addr = 0;
            if (plugin->load_addr == flash_load_addr) {
                abs_addr = flash_load_addr + rel_addr;
                int rv=0;
                if ((rv = hw_flash_erase((uint32_t)abs_addr, data_len, block_log_size))) {
                    infof("ERROR:erase:%08x:%d:%d\n", abs_addr, data_len, block_log_size);
                }
                if ((rv = hw_flash_write((uint32_t)abs_addr, data_buf, data_len))) {
                    infof("ERROR:write:%08x\n", abs_addr);
                }
            }
            else if (plugin->load_addr == ram_load_addr) {
                abs_addr = ram_load_addr + rel_addr;
                memcpy(abs_addr, data_buf, data_len);
            }
            else {
                infof("load_addr %08x doesn't match Flash %08x or RAM %08x\n",
                      plugin->load_addr, &_eflash, &_ebss);
                goto not_handled;
            }
            infof("R:%08x -> A:%08x\n", rel_addr, abs_addr);
            goto handled;
        }
        }
        break;
    }
#if 1
    case TAG_PLUGIO: {
        struct plugin_service *s;
        if ((s = plugin_started()) && s->io.write) {
            s->io.write(buf+2, len-2);
        }
        goto handled;
        break;
    }

#endif

    case TAG_FLASH_ERASE:
    case TAG_FLASH_WRITE:
        flash_handle_message(buf, len);
        goto handled;

    }
  not_handled:
    return 0;
  handled:
    return len;
  bad_plugin:
    infof("bad plugin\n");
    goto handled;

}



/* Small utility to print out memory usage. */
int flash_used(uint8_t *block, uint32_t page_size) {
    uint8_t acc = 0xFF;
    for(uint32_t b=0; b<page_size; b++) {
        acc &= block[b];
    }
    return acc != 0xFF;
}
void info_flash(uint32_t addr, uint32_t page_size, uint32_t nb_pages) {
    infof("addr     0123456789abcdef\n");
    for(uint32_t p=0; p<nb_pages; p++) {
        if ((p % 16) == 0) {
            infof("%x ", addr);
        }
        char c = flash_used((void*)addr, page_size) ? 'x' : '.';
        info_putchar(c);
        addr += page_size;
        if ((p % 16) == 15) {
            infof("\n");
        }
    }
}
