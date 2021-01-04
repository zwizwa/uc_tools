#ifndef TAG_U32_H
#define TAG_U32_H


#include "packet_tags.h"
#include "byteswap.h"
#include "log.h"
#include <stdint.h>

/* What is a good simple protocol to send commands to a
   microcontroller that is programmed in C?  Usually, protocol
   constraints are imposed externally, but I've found that in 99% of
   ad-hoc cases, all I want is an array of u32 values plus some opaque
   payload.  This maps to C function calls in the most straightforward
   way.  TAG_U32 is reserved in packet_tags.h

   For RPC, the continuation can be embedded in the binary payload.
   It doesn't seem appropriate to standardize that here.
*/

struct tag_u32;
typedef int (*tag_u32_handle_fn)(struct tag_u32 *);
typedef void (*tag_u32_reply_fn)(const struct tag_u32 *, const struct tag_u32 *);

struct tag_u32 {
    void *context;
    /* Reply address for RPC calls. */
    const uint32_t* from;  uint32_t nb_from;
    /* Destination address + tags.  Can also contain data. */
    const uint32_t* args;  uint32_t nb_args;
    /* Opaque payload, described by tags. */
    const uint8_t*  bytes; uint32_t nb_bytes;
    /* Reply sender.  This is for abstract RPC. */
    tag_u32_reply_fn reply;
};


#define TAG_U32_ERROR_BAD  -1  // Used as a generic "bad command" code
#define TAG_U32_ERROR_SIZE -2  // Inconsistent size fields


int tag_u32_dispatch(tag_u32_handle_fn handler,
                     tag_u32_reply_fn reply,
                     void *context,
                     const uint8_t *buf, uint32_t nb_buf);

static inline void send_reply_tag_u32_maybe(
    const struct tag_u32 *req, const struct tag_u32 *rpl) {
    if (req->reply) {
        //LOG("tag_u32_reply ok\n");
        req->reply(req, rpl);
    }
    else {
        LOG("tag_u32_reply not supported\n");
    }
}

#define SEND_REPLY_TAG_U32(req, ...) {                          \
        uint32_t a[] = { __VA_ARGS__ };                         \
        const struct tag_u32 s = {                              \
            .args = a, .nb_args = sizeof(a)/sizeof(uint32_t),   \
        };                                                      \
        send_reply_tag_u32_maybe(req, &s);                      \
}

/* This assumes reply_tag_u32 supports req==NULL to send a plain message. */
#define SEND_TAG_U32(...) SEND_REPLY_TAG_U32(NULL, __VA_ARGS__)


static inline void send_reply_tag_u32_status_cstring(
    const struct tag_u32 *req, uint32_t status, const char *string) {
    const struct tag_u32 s = {
        .args = &status, .nb_args = 1,
        .bytes = (const uint8_t*)string,
        .nb_bytes = strlen(string)
    };
    send_reply_tag_u32_maybe(req, &s);
}




/* Note that send_tag_u32() which will have to be defined by the
   firmware image.  See mod_send_tag_u32.c for an implementation that
   sends over SLIP. */
void send_tag_u32(const struct tag_u32 *);
void send_reply_tag_u32(const struct tag_u32 *, const struct tag_u32 *);


/* Pattern matching macros.

   Note that these just behave as if clauses, so will need to contain
   a return or jump statement to break off a sequence of clauses.

   Size comparison is >=, so this can be used to match the head of a
   message.  Note that this does silently ignore arguments.  Maybe
   make it explicit?
*/

/* This uses a C99 for clause to create a local binding environment.
   To go through the loop only 1 times in case of a match, the pointer
   is set to 0. */
#define TAG_U32_MATCH(_req, _tag, _m,  ...)                     \
    for(const struct { uint32_t __VA_ARGS__; } *_m =            \
            (const void*)(&(_req)->args[1]);                    \
        ((_req)->nb_args >= 1 + (sizeof(*_m)/4)) &&             \
            (_tag == (_req)->args[0]) && _m;                    \
        _m=0)

/* This needs to be special-cased. */
#define TAG_U32_MATCH_0(_req, _tag)                             \
    if (((_req)->nb_args >= 1) && ((_req)->args[0] == _tag))

#define TAG_U32_SHIFT(r,n) \
    { .args = (r)->args+(n), .nb_args = (r)->nb_args-(n) }

/* Imperative enter/leave for composing handlers without much memory
   overhead. */
static inline void tag_u32_shift(struct tag_u32 *r, int n) {
    r->args += n;
    r->nb_args -= n;
}
static inline void tag_u32_enter(struct tag_u32 *r) { tag_u32_shift(r, 1); }
static inline void tag_u32_leave(struct tag_u32 *r) { tag_u32_shift(r, -1); }


/* Control protocol, for metadata discovery. */

/* Control tag is used for retrieving metadata for a particular node. */
#define TAG_U32_CTRL 0xFFFFFFFF

/* It has the following RPC requests defined as sub-tags: */
//#define TAG_U32_CTRL_NB_NODES  0  /* Get nb of sub nodes at this node. */
//#define TAG_U32_CTRL_NODE_ID   1  /* Get node id by node list index. */
#define TAG_U32_CTRL_ID_NAME   2  /* Map identifier to name */
#define TAG_U32_CTRL_ID_TYPE   3  /* Map identifier to type */
#define TAG_U32_CTRL_NAME_ID   4  /* Map name to identifier. */


struct tag_u32_entry {
    const char *name;
    const char *type;
    int nb_args; // negative is unspecified
    tag_u32_handle_fn handle;
};

int handle_tag_u32_map(struct tag_u32 *r, const struct tag_u32_entry *map, uint32_t nb_entries);

#define HANDLE_TAG_U32_MAP(r, map) \
    handle_tag_u32_map(r, map, ARRAY_SIZE(map))

#endif
