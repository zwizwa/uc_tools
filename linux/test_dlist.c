#include "macros.h"
#include <stdint.h>

struct u32;
struct u32 {
    struct u32 *next;
    struct u32 *prev;
    uint32_t   value;
};
typedef struct u32 u32_t;

#define NS(name) u32##name
#include "ns_dlist.h"
#undef NS


static void log_list(const char *label, u32_t *l) {
    LOG("%-28s [", label);
    for (u32_t *it = l->next; it != l; it = it->next) {
        LOG(it == l->next ? "%u" : " %u", it->value);
    }
    LOG("]\n");
}



int main(int argc, char **argv) {

    u32_t list;
    u32_init(&list);
    log_list("init:", &list);

    /* static node pool */
    u32_t node[6] = {};
    for (uint32_t i = 0; i < 6; i++) {
        node[i].value = i * 10;
    }
    /* head inserts: 20 10 0 */
    for (int i = 0; i < 3; i++) {
        u32_push_head(&list, &node[i]);
    }
    log_list("push_head 0,10,20:", &list);

    /* tail inserts: 20 10 0 30 40 50 */
    for (int i = 3; i < 6; i++)
        u32_push_tail(&list, &node[i]);
    log_list("push_tail 30,40,50:", &list);

    /* remove from the middle by handle — no traversal */
    u32_remove(&node[0]);   /* value 0 */
    log_list("remove(0):", &list);
    u32_remove(&node[4]);   /* value 40 */
    log_list("remove(40):", &list);

    /* LRU-style touch: move 30 to the head */
    u32_remove(&node[3]);
    u32_push_head(&list, &node[3]);
    log_list("touch(30):", &list);

    /* pop both ends */
    u32_t *e;
    e = u32_pop_head(&list);
    LOG("%-28s %u\n", "pop_head ->", e->value);
    e = u32_pop_tail(&list);
    LOG("%-28s %u\n", "pop_tail ->", e->value);
    log_list("after pops:", &list);

    /* drain */
    while ((e = u32_pop_head(&list)))
        LOG("%-28s %u\n", "drain pop_head ->", e->value);
    log_list("drained:", &list);
    LOG("empty: %s\n", list.next == &list ? "yes" : "no");

    /* re-insert after drain works: POST invariant left next==0 */
    u32_push_tail(&list, &node[2]);
    log_list("reuse node 20:", &list);

    return 0;
}
