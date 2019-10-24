/* Code for handling plugin load & start */
#include "plugin_api.h"
#include "pbuf.h"

#include "memory.h"


int plugin_active = 0;
struct plugin_service *plugin_header;

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
    if (!plugin_active) return 0;
    return _eflash.io.read(buf, len);
}

/* Handle all plugin related messages. */
uint32_t plugin_write_message(const uint8_t *buf, uint32_t len) {
    uint16_t tag = read_be(buf, 2);
    switch(tag) {

    case TAG_PLUGCTL:
        if (len >= 3) {
            switch(buf[2]) {
                case 0:
                    // Note that this message won't get to the host if
                    // we crash in the function call.
                    infof("starting plugin: 0x%08x\n", _eflash.start);
                    _eflash.start();
                    plugin_active = 1;
                    goto handled;
            }
        }
        break;
    case TAG_PLUGIO:
        if (_eflash.version != PLUGIN_API_VERSION) {
            infof("bad plugin api %08x", _eflash.version);
        }
        else {
            _eflash.io.write(buf+2, len-2);
            goto handled;
        }
        break;
    /* These are not implemented by RPC to keep implementation simple
     * and to avoid round-trip delays.  At the end of a programming
     * operation, send a ping to synchronize.  The application should
     * ensure no messages are interleaved that would see a partially
     * programmed flash state. */

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
        if (req_addr == 0) {
            /* If host doesn't give us an actual address, we're going
               to assume a couple of things.

               1.  We can use map_addr to map relative addresses into
                   the destination space.

               2.  The first block is going to contain a plugin header.
            */
            struct plugin_service *plugin = (void*)data_buf;
            infof("plugin->version   = 0x%08x\n", plugin->version);
            infof("plugin->load_addr = 0x%08x\n", plugin->load_addr);

            /* Keep a copy of the pointer so we can read it out on the
               next iteration. */
            plugin_header = plugin->load_addr;
        }

        uint32_t addr = map_addr(req_addr);
        uint32_t data_len  = len - 6;
        int rv = hw_flash_write(addr, data_buf, data_len);
        //if (rv) {
            infof("w:%08x:%d:%d\n", addr, data_len, rv);
        //}
        goto handled;
    }
    }
  //not_handled:
    return 0;
  handled:
    return len;
}

