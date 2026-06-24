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

#define DIR_TRAVERSE_MAX_DEPTH 3
struct dir_traverse {
    void *next;
    struct dir_cursor path[DIR_TRAVERSE_MAX_DEPTH];
    int depth;
};

#define _DIR_TRAVERSE_YIELD(s,label) \
    do { s->next = &&label; return; label: (void)0; } while(0)

#define DIR_TRAVERSE_YIELD(s)                   \
    _DIR_TRAVERSE_YIELD(s,GENSYM(label_))

void dir_traverse_init(struct dir_traverse *s,
                       const char *top) {
    memset(s,0,sizeof(*s));
    for(int i=0; i<ARRAY_SIZE(s->path); i++) {
        s->path[i].dir_fd = -1;
    }
    s->path[0].dir = opendir(top);
    ASSERT(s->path[0].dir);
    s->path[0].dir_fd = dirfd(s->path[0].dir);
}
void dir_traverse_next(struct dir_traverse *s) {
    if (s->next) goto *s->next;
    for(;;) {
        struct dir_cursor *c = &s->path[s->depth];
        c->entry = readdir(c->dir);
        if (!c->entry) {
            /* Last entry: pop or exit */
            closedir(c->dir);
            c->dir = NULL;
            c->dir_fd  = -1;
            if (s->depth == 0) {
                break;
            }
            else {
                s->depth--;
                continue;
            }
        }

        const char *name = c->entry->d_name;
        if (!strcmp(".",  name)) continue;
        if (!strcmp("..", name)) continue;

        /* Valid entry. */
        if (s->depth+1 < ARRAY_SIZE(s->path)) {
            /* We have room to descend. */
            struct dir_cursor *c1 = &s->path[s->depth + 1];
            int fd = c1->dir_fd =
                openat(c->dir_fd,
                       name,
                       O_RDONLY | O_DIRECTORY);
            if (fd != -1) {
                // LOG("openat ok %s\n", name);
                if ((c1->dir = fdopendir(fd))) {
                    /* It is a directory, we can recurse. */
                    // LOG("into %s\n", name);
                    s->depth++;
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
        DIR_TRAVERSE_YIELD(s);
    }
    LOG("EOF\n");
    s->depth = -1; // EOF marker
    for(;;) {
        DIR_TRAVERSE_YIELD(s);
    }
}



void dir_traverse(
    const char *top,
    uintptr_t max_depth,
    void (*visit)(struct dir_cursor *dc, void *ctx, uintptr_t depth),
    void *ctx) {
    struct dir_traverse _s, *s = &_s;
    dir_traverse_init(s, top);
    for (;;) {
        dir_traverse_next(s);
        if (s->depth < 0) break;
        visit(s->path, ctx, s->depth);
    }
}


void visit(struct dir_cursor *dc, void *ctx, uintptr_t depth) {
    for (uintptr_t i=0; i<=depth; i++) {
        LOG(" %d", i);
        ASSERT(dc[i].entry);
        LOG(" %s", dc[i].entry->d_name);
    }
    LOG("\n");
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    dir_traverse(".", DIR_TRAVERSE_MAX_DEPTH, visit, NULL);

}
 
