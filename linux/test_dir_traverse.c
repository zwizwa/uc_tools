/* Example: how to traverse a directory tree. */

#include "dir_traverse.h"


void visit(struct dir_cursor *dc, void *ctx, uintptr_t depth) {
    if (depth != 2) return;
    for (uintptr_t i=0; i<=depth; i++) {
        // LOG(" %d", i);
        ASSERT(dc[i].entry);
        LOG(" %s", dc[i].entry->d_name);
    }
    LOG("\n");
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    dir_traverse(".", DIR_TRAVERSE_MAX_DEPTH, visit, NULL);

}

