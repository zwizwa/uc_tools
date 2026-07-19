#include "macros.h"
#include <stdint.h>

struct u32_lru;
struct u32_lru {
    struct u32_lru *next;
    struct u32_lru *prev;
    struct u32_lru *sentinel;
    uint32_t        value;
};
typedef struct u32_lru u32_lru_t;

#define NS(name) u32##name
#include "ns_lru.h"
#undef NS


static void log_list(const char *label, u32_lru_t *l) {
    if (!l->next) {
        /* Non-linked element. */
        LOG("%-14s (%u)\n", label, l->value);
    }
    else {
        /* Actual list */
        LOG("%-14s [", label);
        for (u32_lru_t *it = l->next; it != l; it = it->next) {
            LOG(it == l->next ? "%u" : " %u", it->value);
        }
        LOG("]\n");
    }
}



int main(int argc, char **argv) {

    u32_lru_t list;
    u32_lru_init(&list);
    log_list("init:", &list);

    /* static node pool */
    u32_lru_t node[6] = {};
    for (uint32_t i = 0; i < 6; i++) {
        node[i].value = i;
    }

    /* Initialize in sequence. */
    for (int i = 0; i < 6; i++) {
        u32_lru_push_head(&list, &node[i]);
    }
    log_list("push_head all:", &list);

    u32_lru_use(&node[3]);
    log_list("use 3:", &list);

    u32_lru_use(&node[0]);
    log_list("use 0:", &list);

    u32_lru_t *e;
    e = u32_lru_pop_tail(&list);
    log_list("reclaimed:", e);
    log_list("left:", &list);

    e = u32_lru_pop_tail(&list);
    log_list("reclaimed:", e);
    log_list("left:", &list);

    return 0;
}
