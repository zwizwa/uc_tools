/* Attempt at some generic query browser.

   The idea is to point this at a DB file with some metadata exposing
   structure and this program will present a TUI browser.

   - I wanted to use this for a single (file) list but that is a bit
     overkill.  Use dialog for that.

   - For multi-level, the levels can be keys: top key, second key
     etc...  I need an actual example to use this.

*/

#define _GNU_SOURCE
#include <stdio.h>

#define MMAP_BUMP_MAX_ALIGN sizeof(uintptr_t)

/* Generic code.  All the rest is code that depends on the database
   schema and contents. */
#include "mod_query_browser.c"

/* Arena-allocator based data structures tied into sql queries. */
#include "mod_sqlite3_data.c"

/* Database should expose at least this top level table. */

/* The C struct and constructors -- copy constructor + from_query
   constructor -- are generated using macros. */
#define TOP(m)    \
    m(0, text, entry) \
    m(1, text, action) \

DEF_TABLE(top, TOP)

/* The SQL iteration doesn't know the number of rows in advance, but
   we do need to build an O(1) index for the TUI. This is solved by
   creating a separate array structure after traversing the table.
   The table structs have a .next field that can be used to keep track
   of the instances during traversal. */

/* The toplevel state struct. */
struct ui_state;
struct ui_behavior {
    /* State transitions. */
    struct ui_state *(*enter)(struct arena *a, struct ui_state *s, uintptr_t index);
    /* View */
    void (*format)(struct ui_state *s, uintptr_t index, char *buf, uintptr_t buf_size);
};

struct ui_state {
    uintptr_t nb_rows;
    struct ui_behavior behavior;
    union {
        struct top **top;
    } table;
    int exit:1; // request exit
    const char *action;
};


static void format_top(
    struct ui_state *s,
    uintptr_t index,
    char *buf,
    uintptr_t buf_size)
{
    ASSERT(index < s->nb_rows);
    struct top *t = s->table.top[index];
    snprintf(buf, buf_size, "%s", t->entry);
}
// This is "press enter while inside top_table list"
static struct ui_state *enter_top(
    struct arena *a,
    struct ui_state *s,
    uintptr_t index)
{
    /* If there is a second level, see query_browser.c
       enter_report_table() on how to procees.  In first iteration we
       just excute action as a shell command and be done. */
    struct top *t = s->table.top[index];
    s->action = t->action;
    s->exit = 1;
    return s;
}



struct ui_state *ui_state_top(struct arena *a) {

    struct ui_state *s = arena_alloc0(a, sizeof(*s));

    s->behavior.format = format_top;
    s->behavior.enter  = enter_top;

    s->nb_rows = 0;
    /* The mod_sqlite3 stmt() function can cache prepared statements.
       Follow this static variable pattern. */
    static sqlite3_stmt *q;
    stmt(&q, "SELECT * from top");
    int rv;
    struct top *last = NULL;
    while (SQLITE_ROW == (rv = sqlite3_step(q))) {
        struct top *t = top_from_query(a, q);
        s->nb_rows++;
        t->prev = last;
        last = t;
    }
    ASSERT(rv == SQLITE_DONE);

    /* Create the index array from the linked structure created in the
       query traversal. */
    s->table.top = arena_alloc(a, sizeof(*last) * s->nb_rows);
    for(int i = s->nb_rows-1; i >=0; i--) {
        s->table.top[i] = last;
        last = last->prev;
    }
    return s;
}


/* If there is no current state then start at the top level.  All the
   other transitions are going to leave qb->state set up so tui can
   use it. */
void qb_need_state(struct query_browser *qb) {
    if (qb->state) return;
    qb->state = ui_state_top(&qb->arena);
    ASSERT(qb->state);
}


void qb_format(struct query_browser *qb,
               uintptr_t index,
               char *buf,
               uintptr_t buf_size) {
    qb_need_state(qb);
    struct ui_state *s = qb->state;
    s->behavior.format(s, index, buf, buf_size);
}


uintptr_t qb_nb_rows(struct query_browser *qb) {
    qb_need_state(qb);
    struct ui_state *s = qb->state;
    return s->nb_rows;
}

int qb_enter(struct query_browser *qb, uintptr_t index) {
    qb_need_state(qb);
    struct ui_state *s = qb->state;
    qb->state = s->behavior.enter(&qb->arena, s, index);
    return !s->exit;
}


int main(int argc, char **argv) {
    ASSERT(argc == 2);
    const char *db = NULL;
    db = argv[1];
    const char *ext[] = {
        // "./csv.so",
        NULL,
    };
    struct ui_state *s = qb_loop(db,ext);
    LOG("action: %s\n", s->action);
    // Note that stdout cannot be used becuase it is used by ncurses.
    // It is possible to use stderr:
    // https://claude.ai/chat/1e93f000-4956-473f-95e3-c7d0b1a4769a

}
