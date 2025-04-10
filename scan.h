#ifndef SCAN_H
#define SCAN_H

#include <stdint.h>

/* +- strtok replacement */
struct scan {
    char *str;
    uintptr_t i;
    uintptr_t n;
    int dup;
    char sep;
};

/* The string is processed in-place. */
static inline void scan_init(struct scan *s, char *str, char sep) {
    s->str = str;
    s->n = strlen(str);
    s->sep = sep;
    s->i = 0;
    /* Allow duplicate separates between elements, any number of
       separators at start, and any number of terminators at
       end. This seeems to be all I need. */
    s->dup = 1;
};

static inline char *scan_next(struct scan *s) {
    char sep = s->sep;
    char *str = s->str;
    uintptr_t n = s->n;
    uintptr_t i = s->i;
    if (i == n) {
        /* Last scan went to the end. */
        return NULL;
    }

    /* Skip duplicate separators at start if requested. */
    if (s->dup) {
        while((i < n) && (str[i] == sep)) i++;
    }
    s->i = i;

    for(;;) {
        if (i == n) {
            /* End of string. */
            goto done;
        }
        if (str[i] == sep) {
            str[i] = 0;
            i++;
            /* Skip other separators if requested. */
            if (s->dup) {
                while((i < n) && (str[i] == sep)) i++;
            }
            goto done;
        }
        i++;
    }
  done:
    char *rv = &str[s->i];
    s->i = i;
    return rv;
}


/* Other misc scanning ops. */

typedef void (*text_for_lines_fn)(void *context, const char *line);

static inline void text_for_lines(
    text_for_lines_fn fn, void *context,
    const uint8_t *buf, uintptr_t len,
    uintptr_t max_line_len)
{
    char line[max_line_len+1];
    int n = 0;
    for(;;) {
        if (len == 0) {
            /* Parse whatever we have left. */
            line[n] = 0;
            if (n > 0) {
                fn(context, line);
            }
            return;
        }
        if (n < max_line_len) {
            uint8_t c = *buf++; len--;
            if ((c == '\r') || (c == '\n')) {
                line[n] = 0;
                fn(context, line);
                n = 0;
                if ((c == '\r') && (len > 0) && ('\n' == *buf)) {
                    // support \r\n terminator as well
                    buf++; len--;
                }
            }
            else {
                line[n++] = c;
            }
        }
    }
}



#endif
