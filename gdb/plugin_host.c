/* Code for handling plugin load & start */
#include "plugin_api.h"
#include "pbuf.h"

#include "memory.h"


int plugin_active = 0;
struct plugin_service *plugin_service = 0;

static int plugin_ok(void) {
    return plugin_service
        && (plugin_service->version == PLUGIN_API_VERSION)
        && plugin_active;
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
    if (!plugin_ok()) return 0;
    return plugin_service->io.read(buf, len);
}

/* Handle all plugin related messages. */

/* Note that the messages are not implemented by RPC to keep
   implementation simple and to avoid round-trip delays.  At the end
   of a programming operation, send a ping to synchronize.  The
   application should ensure no messages are interleaved that would
   see a partially programmed flash state. */

uint32_t plugin_write_message(const uint8_t *buf, uint32_t len) {
    uint16_t tag = read_be(buf, 2);
    switch(tag) {

    case TAG_PLUGCTL: {
        uint16_t cmd = read_be(buf+2, 2);
        switch(cmd) {
        case 0:  // START
            // Note that this message won't get to the host if
            // we crash in the function call.
            if (plugin_service) {
                infof("starting plugin: 0x%08x\n", plugin_service->start);
                plugin_service->start();
                plugin_active = 1;
                goto handled;
            }
            else {
                /* FIXME: There could be one in flash. */
                infof("plugin_service == NULL\n");
            }
        case 1: { // WRITE BLOCK
            void *ram_load_addr = &_ebss;
            void *flash_load_addr = &_eflash;

            plugin_active = 0;
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

                /* Keep a copy of the pointer so we can read it out on the
                   next iteration. */
                plugin_service = plugin->load_addr;
            }
            else {
                plugin = plugin_service;
            }
            /* We're going to constrain loading to where we expect
             * data to be going. */
            void *abs_addr;
            if (plugin->load_addr == flash_load_addr) {
                abs_addr = flash_load_addr + rel_addr;
                int rv;
                if ((rv = hw_flash_erase((uint32_t)abs_addr, data_len, block_log_size))) {
                    infof("ERROR:erase:%08x\n", abs_addr);
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
                infof("load_addr doesn't match Flash %08x or RAM %08x\n",
                      &_eflash, &_ebss);
                goto not_handled;
            }
            infof("R:%08x -> A:%08x\n", rel_addr, abs_addr);
            goto handled;
        }
        }
        break;
        }
    case TAG_PLUGIO:
        if (plugin_ok()) {
            plugin_service->io.write(buf+2, len-2);
            goto handled;
        }
        break;

    // bp4 ! {send_packet,<<16#FFF6:16,16#08005000:32, 1024:32, 10:32>>}.
    case TAG_FLASH_ERASE: {
        plugin_active = 0;
        uint32_t addr = map_addr(read_be(buf+2,  4));
        uint32_t size = read_be(buf+6,  4);
        uint32_t log  = read_be(buf+10, 4);
        int rv = hw_flash_erase(addr, size, log);
        //if (rv) {
            infof("e:%08x:%d:%d:%d\n", addr, size, log, rv);
        //}
        goto handled;
    }
    // bp4 ! {send_packet,<<16#FFF7:16,16#08005000:32,1,2,3,4>>}.
    case TAG_FLASH_WRITE: {
        plugin_active = 0;
        uint32_t req_addr = read_be(buf+2, 4);
        const uint8_t *data_buf  = &buf[6];
        uint32_t addr = map_addr(req_addr);
        uint32_t data_len  = len - 6;
        int rv = hw_flash_write(addr, data_buf, data_len);
        //if (rv) {
            infof("w:%08x:%d:%d\n", addr, data_len, rv);
        //}
        goto handled;
    }
    }
  not_handled:
    return 0;
  handled:
    return len;
}

