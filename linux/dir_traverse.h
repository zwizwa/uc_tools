#ifndef DIR_TRAVERSE_H
#define DIR_TRAVERSE_H

#include "macros.h"
#include <stdio.h>
#include <dirent.h>
#include <stdint.h>


struct dir_cursor {
    int dir_fd;
    DIR *dir;
    struct dirent *entry;
};

/* It's ok to keep this static.  The original puprose of this code is
   not arbitrary depth recursion but mapping directory structures to
   fixed width tables inside sqlite virtual table implementations. */
#ifndef DIR_TRAVERSE_MAX_DEPTH
#define DIR_TRAVERSE_MAX_DEPTH 3
#endif



struct dir_traverse {
    void *next;
    struct dir_cursor path[DIR_TRAVERSE_MAX_DEPTH];
    int depth;     // current depth for traversal state machine
    int end_depth;
    const char *ext;
};

#define _DIR_TRAVERSE_YIELD(s,label) \
    do { s->next = &&label; return; label: (void)0; } while(0)

#define DIR_TRAVERSE_YIELD(s)                   \
    _DIR_TRAVERSE_YIELD(s,GENSYM(label_))

static void dir_traverse_init(struct dir_traverse *s,
                              const char *top,
                              int end_depth) {
    memset(s,0,sizeof(*s));
    s->end_depth = end_depth;
    for(int i=0; i<s->end_depth; i++) {
        s->path[i].dir_fd = -1;
    }
    s->path[0].dir = opendir(top);
    ASSERT(s->path[0].dir);
    s->path[0].dir_fd = dirfd(s->path[0].dir);
}

static inline int dir_traverse_ext_match(struct dir_traverse *s) {
    if (!s->ext) return 1; // no filtering
    const char *filename = s->path[s->depth].entry->d_name;

    int ext_len = strlen(s->ext); // FIXME hoist
    int filename_len = strlen(filename);
    if (filename_len < ext_len) return 0;
    const char *filename_ext = filename + (filename_len - ext_len);
    return !strcmp(filename_ext, s->ext);
}

static void dir_traverse_close(struct dir_traverse *s) {
    for(int i=0; i<s->end_depth; i++) {
        struct dir_cursor *c = &s->path[i];
        if (c->dir) {
            // LOG("closing %d\n", i);
            // This also closes the fd
            closedir(c->dir);
            c->dir = NULL;
        }
        else {
            // LOG("already closed %d\n", i);
        }
        c->dir_fd = -1;
    }
}

static void dir_traverse_next(struct dir_traverse *s) {
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
        if (s->depth+1 < s->end_depth) {
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
        if (dir_traverse_ext_match(s)) {
            DIR_TRAVERSE_YIELD(s);
        }
    }
    // LOG("EOF\n");
    s->depth = -1; // EOF marker
    for(;;) {
        DIR_TRAVERSE_YIELD(s);
    }
}
static int dir_traverse_end(struct dir_traverse *s) {
    return s->depth < 0;
}


static inline void dir_traverse(
    const char *top,
    uintptr_t max_depth,
    void (*visit)(struct dir_cursor *dc, void *ctx, uintptr_t depth),
    void *ctx) {
    struct dir_traverse _s, *s = &_s;
    dir_traverse_init(s, top, DIR_TRAVERSE_MAX_DEPTH);
    // s->ext = ".ilog";
    s->ext = ".bin";
    for (;;) {
        dir_traverse_next(s);
        if (dir_traverse_end(s)) break; 
        visit(s->path, ctx, s->depth);
    }
    dir_traverse_close(s);
}

#endif
