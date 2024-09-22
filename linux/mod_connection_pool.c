#ifndef MOD_CONNECTION_POOL
#define MOD_CONNECTION_POOL

// FIXME: Lifted from unbrickable router daemon, which is
// reimplemented to use sqlite to store the connections.

/* CONNECTION POOL */
struct connection;
struct connection {
    void (*handle)(struct connection *);
    int fd;
    short revents;
};
struct pool {
    struct connection *conn;
    size_t used;
    size_t room;
    size_t cursor;
};

/* The pool is implemented as a packed array of connections.  To
   simplify the datastructure, the pool is modified (add, delete)
   while traversing.  The pollfd is regenerated for each poll() call,
   and the result of the poll() call is copied into the connection
   array, so it can be rearranged while traversing. */

struct pool *pool_new(size_t room) {
    ASSERT(room > 0);
    struct pool *p = malloc(sizeof(*p));
    memset(p, 0, sizeof(*p));
    p->conn = malloc(sizeof(struct connection) * room);
    ASSERT(p->conn);
    p->used = 0;
    p->room = room;
    return p;
}
void pool_grow(struct pool *p) {
    p->room *= 2;
    ASSERT(p->conn = realloc(p->conn, sizeof(struct connection) * p->room));
}
struct connection *pool_alloc(struct pool *p) {
    if (p->used == p->room) pool_grow(p);
    p->used++;
    struct connection *c = &p->conn[p->used - 1];
    c->fd = -1;
    return c;
}
struct connection *pool_current(struct pool *p) {
    if (p->cursor < p->used) return &p->conn[p->cursor];
    return NULL;
}
void pool_rewind(struct pool *p) {
    p->cursor = 0;
}
void pool_next(struct pool *p) {
    ASSERT(p->cursor < p->used);
    p->cursor++;
}
void pool_delete(struct pool *p) {
    /* Note that we always delete connections in response to errors
       while traversing the pool. */
    struct connection *c = &p->conn[p->cursor];

    close(c->fd); // FIXME

    if (p->cursor == (p->used - 1)) {
        /* If it's the last one, just drop it.  Iteration will end here. */
        ASSERT(p->used > 0);
        p->used--;
    }
    else {
        /* Otherwise, move last one here.  Iteration will continue in
           the current slot. */
        *c = p->conn[p->used - 1];
        p->used--;
    }
}

struct pool *pool;


#endif
