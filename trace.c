/* Reuse the info_putchar() buffer to support binary messages.  This
   encodes tag_u32 style messages in the info buffer using SLIP.
*/

#include "infof.h"
#include "slip.h"
#include "cycle_counter.h"
#include <string.h>
#include <stdarg.h>

/* The slip buffer size is 2x orig_size to handle the worst case. */
uintptr_t slip_encode(uint8_t *slip, const void *_orig, uintptr_t orig_size) {
    uintptr_t nb = 0;
    const uint8_t *orig = _orig;
    for(uintptr_t i=8; i<orig_size; i++) {
        uint8_t c = orig[i];
        if (c == SLIP_END) {
            *slip++ = SLIP_ESC;
            *slip++ = SLIP_ESC_END;
            nb+=2;
        }
        else if (c == SLIP_ESC) {
            *slip++ = SLIP_ESC;
            *slip++ = SLIP_ESC_ESC;
            nb+=2;
        }
        else {
            *slip++ = c;
            nb++;
        }
    }
    return nb;
}

static inline int is_digit(int d) {
    return d >= '0' && d <= '9';
}

/* vinfof has two behavors depending on configuration.  One is to
   produce a binary encoding, the other is to produce text, similar to
   infof. */

int vtracef_text(uint32_t tag_ignored, const char *fmt, va_list ap) {
    vinfof_with_escape('_', fmt, ap);
    infof("\n");
    return 0;
}

int vtracef_binary(uint32_t tag, const char *fmt, va_list ap) {
    {
        /* Start of packet + header with timestamp and log message tag. */
        uint32_t hdr[] = {cycle_counter(), tag};
        uint8_t slip[1+2*sizeof(hdr)] = { SLIP_END };
        uintptr_t nb = 1 + slip_encode(slip + 1, hdr, sizeof(hdr));
        info_write(slip, nb);
    }

    while (*fmt) {
        switch(*fmt) {
        case '_': {
            fmt++;

            /* Ignore '0'; Strings are inlined, everything else is
               encoded as uint32_t. We can ignore all number size
               formatting. */
            while (is_digit(*fmt));
            switch(*fmt) {
            case 0: {
                break;
            }
            case 's': {
                const char *str = va_arg(ap,const char*);
                uintptr_t len = strlen(str);
                uint8_t slip[len*2];
                uintptr_t nb = slip_encode(slip, str, len);
                info_write(slip, nb);
                fmt++;
                break;
            }
            default: {
                uint32_t val = va_arg(ap, uint32_t);
                uint8_t slip[2*sizeof(val)];
                uintptr_t nb = slip_encode(slip, &val, sizeof(val));
                info_write(slip, nb);
                fmt++;
                break;
            }
            break;
        }
        default:
            /* Drop all name info here. */
            fmt++;
            break;
        }
        }
    }
    {
        /* This ensures that the "junk" between slip-encoded packets
         * (e.g. ordinary log data) is also packet-framed, that is as
         * long as infof() and friends are not used to send the
         * encoding characters.  Normal 7 bit ASCII is compliant. */
        uint8_t slip[] = { SLIP_END };
        info_write(slip, 1);
    }
    return 0;
}

int tracef(uint32_t tag, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    // FIXME: How to configure? Should this always be compile time?
    int rv = vtracef_text(tag, fmt, ap);
    va_end(ap);
    return rv;
}

