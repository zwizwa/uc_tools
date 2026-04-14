#include <stdint.h>
extern uint8_t __bss_start;
extern uint8_t __bss_end;

#include "hw_i686.h"
#include "tools.h"
#define memset mini_memset
#define memcpy mini_memcpy

struct log {
    volatile char *video;
    uint8_t col;
    uint8_t row;
    uint8_t nb_rows;
    uint8_t nb_cols;
    uint8_t attrib;
};
void log_init(struct log *log) {
    memset(log, 0, sizeof(*log));
    log->attrib = 0x07;
    log->video = (volatile char *)0xB8000;
    log->nb_cols = 80;
    log->nb_rows = 25;
    /* Start at the bottom to cause a scroll at first character writen. */
    log->col = 0;
    log->row = log->nb_rows;
}

void log_scroll(struct log *log) {
    // Note: mini_memcpy that allows backwards overlapping copy.
    uint32_t row_size     = 2 * log->nb_cols;
    uint32_t rows_m1_size = row_size * (log->nb_rows - 1);
    mini_memcpy_volatile(
        log->video,
        (void*)(log->video + row_size),
        rows_m1_size);
    mini_memset_volatile(
        log->video + rows_m1_size,
        0,
        row_size);
}
static inline uint32_t log_offset(struct log *log) {
    return 2 * (log->nb_cols * log->row + log->col);
}
void log_putchar(struct log *log, char c) {
    while (log->row >= log->nb_rows) {
        log_scroll(log);
        log->col = 0;
        log->row--;
    }
    if (c == '\n') {
        // move to new line
        log->col = 0;
        log->row++;
    }
    else {
        typeof (log->video) v = log->video + log_offset(log);
        *v++ = c;
        *v++ = log->attrib;
        log->col++;
    }
    if (log->col == log->nb_cols) {
        // wrap end-of-line
        log->col = 0;
        log->row++;
    }
}
void log_putstr(struct log *log, char *str) {
    while (*str) {
        log_putchar(log, *str++);
    }
}

// Put everything in a single static struct.
struct app {
    struct log log;
};
void app_init(struct app *app) {
    log_init(&app->log);
    log_putstr(&app->log, "app_init()");
};

struct app app;

__attribute__ ((section (".kmain")))
void kmain(void) {

    // initialize .bss segment to zero
    mini_memset(&__bss_start, 0, &__bss_end - &__bss_start);

#if 0
    // initialize video memory
    volatile char *video = (volatile char *)0xB8000;
    for (int i=0; i<80*25; i++) {
        video[i*2] = '.';
        video[i*2+1] = 0x0F; // white on black
    }
#endif

    // initialize app data
    app_init(&app);

    // enable keyboard interrupt
    outb(0x21, 0b11111101); // only IRQ1 unmasked
    // https://claude.ai/chat/05381f76-0d4a-4083-a1b7-d114e392d17d

  loop:
    hlt();
    
    goto loop;
}
