#ifndef SCAN_H
#define SCAN_H

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

char *scan_next(struct scan *s) {
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



#endif
