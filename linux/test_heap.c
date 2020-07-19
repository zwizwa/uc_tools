#include "heap.h"

void log_buf(uint32_t *buf, uint32_t nb) {
    for (int i=0; i<nb; i++) {  LOG(" %d", buf[i]); }
    LOG("\n");
}
void log_heap(struct heap *h) {
    log_buf(h->buf, h->nb);
}

void test1(int max_nb, int mul, int mod, int log) {
    uint32_t buf[max_nb];
    struct heap h = {
        .nb = 0, .max_nb = max_nb, .buf = buf
    };
    for (int i=0; i<max_nb; i++) {
        int r = (i * mul) % mod;
        heap_insert_bottom(&h, r);
        if (log) log_heap(&h);
    }
    if (log) LOG("\n");
    uint32_t last_top = 0xFFFFFFFF;
    for (int i=0; i<max_nb; i++) {
        uint32_t top = heap_pop(&h);
        if(log) {
            LOG(" %d |", top);
            log_heap(&h);
        }
        ASSERT(last_top >= top);
        last_top = top;
    }
}
void test2(int max_nb, int mul, int mod, int log) {
    uint32_t buf[max_nb];
    for (int i=0; i<max_nb; i++) {
        buf[i] = (i * mul) % mod;
    }
    log_buf(buf, max_nb);
    heap_sort(buf, max_nb);
    log_buf(buf, max_nb);
    uint32_t last = 0;
    for (int i=0; i<max_nb; i++) {
        ASSERT(last <= buf[i]);
        last = buf[i];
    }
}

int main(int argc, char **argv) {
    test1(20, 23, 71, 1);
    test1(20, 7,  13, 1);
    test2(20, 7,  13, 1);
    return 0;
}
