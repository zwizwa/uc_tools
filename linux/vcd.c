#include "macros.h"
#include <stdint.h>
#include "assert_read.h"

/* Adapted from asm_tools VCD.hs

   Currently limited to 8 1-bit signals, raw encoded, hardcoded to
   2MHz sample rate. */

#define TOKEN_START 37
#define TOKEN_ENDX 127
static inline int token(int i) { // zero based to char
    int max = TOKEN_ENDX-TOKEN_START;
    if (i > max) i = max;
    if (i < 0) i = 0;
    return i + TOKEN_START;
}

/* Convert 8-bit logic data to VCD format for GTKWave */
struct vcd_state {
    uintptr_t nb_channels;
    uintptr_t time;
    uintptr_t last;
};

static inline void vcd_header(struct vcd_state *s, uintptr_t first_sample) {
    printf("$date October 21, 2015 $end\n"); // FIXME
    printf("$version VCD.hs $end\n");
    printf("$timescale 500ns $end\n"); // FIXME
    printf("$scope module logic $end\n");
    for(int i=0; i<s->nb_channels; i++) {
        int width = 1;
        char name[] = {'c','0'+i,0};
        printf("$var wire %d %c %s $end\n", width, token(i), name);
    }
    // Hack to make it print the first sample
    s->last = ~first_sample;
}
static inline void vcd_sample(struct vcd_state *s, uintptr_t sample) {
    uintptr_t diff = sample ^ s->last;
    if (diff) {
        printf("#%d\n", s->time);
        for(int i=0; i<s->nb_channels; i++) {
            if ((diff >> i) & 1) {
                int bit = (sample >> i) & 1;
                char line[] = {'0' + bit, token(i), 0};
                puts(line);
            }
        }
    }
    s->last = sample;
    s->time++;
}
static inline void vcd_footer(struct vcd_state *s, uintptr_t nb_samples) {
    printf("#%d\n", nb_samples);
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    struct vcd_state s = { .nb_channels = 8 };
    uint8_t buf[256 * 1024];

    size_t total = 0;
    ssize_t n = assert_read(0, buf, sizeof(buf));
    ASSERT(n > 0);
    vcd_header(&s, buf[0]);
    for(;;) {
        for (int i=0; i<n; i++) {
            vcd_sample(&s, buf[i]);
        }
        total += n;
        n = assert_read(0, buf, sizeof(buf));
        if (n == 0) break;
    }
    vcd_footer(&s, total);
    return 0;
}
