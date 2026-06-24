/* Example: how to traverse a directory tree. */

#include "macros.h"
#include <stdio.h>
#include <dirent.h>
#include <stdint.h>


struct dir_cursor {
    int dir_fd;
    DIR *dir;
    struct dirent *entry;
};

void traverse(const char *top,
              uintptr_t max_depth,
              void (*visit)(struct dir_cursor *dc, void *ctx, uintptr_t depth),
              void *ctx) {

    struct dir_cursor dc[max_depth];
    uintptr_t d = 0;

    dc[d].dir = opendir(top);
    ASSERT(dc[d].dir);
    dc[d].dir_fd = dirfd(dc[d].dir);

    for(;;) {
        dc[d].entry = readdir(dc[d].dir);
        if (!dc[d].entry) {
            /* Last entry: pop or exit */
            closedir(dc[d].dir);
            if (d == 0) {
                break;
            }
            else {
                d--;
                continue;
            }
        }

        const char *name = dc[d].entry->d_name;
        if (!strcmp(".",  name)) continue;
        if (!strcmp("..", name)) continue;

        /* Valid entry. */
        if (d+1 < max_depth) {
            /* We have room to descend. */
            int fd = dc[d+1].dir_fd =
                openat(dc[d].dir_fd,
                       name,
                       O_RDONLY | O_DIRECTORY);
            if (fd != -1) {
                // LOG("openat ok %s\n", name);
                if ((dc[d+1].dir = fdopendir(fd))) {
                    /* It is a directory, we can recurse. */
                    // LOG("into %s\n", name);
                    d++;
                    continue;
                }
                else {
                    /* We already know it's a directory, so this is a
                       real error. */
                    ERROR("opendir failed %s\n", name);
                    close(fd);
                }
            }
            else {
                //LOG("openat failed %s\n", name);
            }
        }
        visit(dc, ctx, d);
    }
}

#define N 3


void visit(struct dir_cursor *dc, void *ctx, uintptr_t depth) {
    for (uintptr_t i=0; i<=depth; i++) {
        LOG(" %s", dc[i].entry->d_name);
    }
    LOG("\n");
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    traverse(".", N, visit, NULL);

}
