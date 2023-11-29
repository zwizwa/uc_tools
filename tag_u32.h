#ifndef TAG_U32_H
#define TAG_U32_H

#include "packet_tags.h"
#include "uct_byteswap.h"
#include "log.h"
#include <stdint.h>

/* TL&DR

   Use a list representation to represent _both_ object hierarchy
   (paths), and method call substructure (method names, arguments,
   type tags for binary payload).

   Make it simple to use symbolic names. */


/* This protocol arose after a search for a good compromise protocol:

   - easy to implement in C with minimial code and memory requirements

   - fast to dispatch in C

   - has both a binary and a C level API

   - self-describing, i.e. including metadata

   - hierarchical: able to describe arbitrary substructure

   - easy to integrate with pattern matching (destructuring) in a
     language like Erlang.

   - easy to map to existing hierarchical data formats that use nested
     tree structures based on symbolic dictionarys (symbol to thing)
     and arrays/lists, e.g. XML, JSON, ...

   The main trick is to not insist on messages as trees, but to split
   the problem up into two layers:

   - messages as tree nodes  (paths + payloads)

   - reliance on transactions spanning multiple tree node messages to
     describe trees

   In short, this protocol can be used to implement the "zipper" tree
   traversal pattern, which allows trees to be implemented as
   iterations over trees, removing the need for explicit tree data
   structure representation at the microcontroller end.

   The protocol is structured as unidirectional messaging in the first
   place, with an RPC extension that uses the same structure for reply
   messages, i.e. by providing a prefix path for RPC replies.

*/



struct tag_u32;
typedef int (*tag_u32_handle_fn)(struct tag_u32 *);
typedef void (*tag_u32_reply_fn)(const struct tag_u32 *, const struct tag_u32 *);

struct tag_u32 {
    void *context;

    /* Reply address for RPC calls. */
    const uint32_t* from;  uint32_t nb_from;

    /* Destination address + tags.  Can also contain data.  Note that
       this one is not const: it is allowed to modify the path
       in-place, i.e. treating it as a stack. */
    uint32_t* args;  uint32_t nb_args;

    /* Opaque payload, described by tags. */
    const uint8_t*  bytes; uint32_t nb_bytes;

    /* Reply sender.  This is for abstract RPC. */
    tag_u32_reply_fn reply; void *reply_ctx;
};


#define TAG_U32_ERROR_BAD  -1  // Used as a generic "bad command" code
#define TAG_U32_ERROR_SIZE -2  // Inconsistent size fields


int tag_u32_dispatch(tag_u32_handle_fn handler,
                     tag_u32_reply_fn reply,
                     void *context,
                     const uint8_t *buf, uint32_t nb_buf);

/* If the sender does not send a from address, we do not send a reply.
   This allows the sender to do sequencing on high-latency links,
   e.g. queueing up a bunch of messages and only requesting a reply
   for the last one. */
static inline void send_reply_tag_u32_maybe(
    const struct tag_u32 *req, const struct tag_u32 *rpl) {
    if (req->nb_from) {
        if (req->reply) {
            //LOG("tag_u32_reply ok\n");
            req->reply(req, rpl);
        }
        else {
            LOG("tag_u32_reply not supported\n");
        }
    }
}

#define SEND_REPLY_TAG_U32_BYTES(req, b, nb, ...) {             \
        uint32_t a[] = { __VA_ARGS__ };                         \
        const struct tag_u32 s = {                              \
            .args = a, .nb_args = sizeof(a)/sizeof(uint32_t),   \
            .bytes = b, .nb_bytes = nb,                         \
        };                                                      \
        send_reply_tag_u32_maybe(req, &s);                      \
}
#define SEND_REPLY_TAG_U32(req, ...) \
    SEND_REPLY_TAG_U32_BYTES(req, NULL, 0, __VA_ARGS__)


/* This assumes reply_tag_u32 supports req==NULL to send a plain message. */
#define SEND_TAG_U32(...) SEND_REPLY_TAG_U32(NULL, __VA_ARGS__)


/* Some wrappers around send_reply_tag_u32_maybe() for often used
 * reply patterns. */

void send_reply_tag_u32_status(
    const struct tag_u32 *req, uint32_t status,
    const uint8_t *bytes, uint32_t nb_bytes);

void send_reply_tag_u32_status_cstring(
    const struct tag_u32 *req, uint32_t status, const char *string);

void send_reply_tag_u32_ok(const struct tag_u32 *req);

void send_reply_tag_u32_ok_u32(const struct tag_u32 *req, uint32_t val);



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

/* Similar, but do not match, just destructure the array into a struct
   at offset.  Guards need to come before this.  This is so
   convenient...  If there is anything you take home, it should be
   this one! */
#define TAG_U32_UNPACK(_req, _offset, _m, ...)                  \
    for(const struct { uint32_t __VA_ARGS__; } *_m =            \
            (const void*)(&(_req)->args[_offset]); _m; _m=0)


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

/* Control tag is used for retrieving metadata for a particular node.

   While it would be more efficient in LEB128 encoding to make this 0,
   there are two reasons why it is not 0.

   - In practice, most directory structures will map to C arrays.  If
     the control tag would be 0, this would mean that the first real
     entry in a map would be at 1.  We do not want to introduce
     1-based indexing horror from the start.

   - If it's not 0, and we want to allow 0 to n-1 array indices, the
     most generic tag to pick is 0xFFFFFFFF.

   - Even in LEB128 where 0xFFFFFFFF requires 5 bytes to encode, the
     inefficiency isn't a big problem because it is only needed when
     bootstrapping the protocol dictionary, which in most cases can
     even be cached at compile time (e.g. caller can obtain the
     callee's dictionary through other means, e.g. some kind of
     version string that then can resolve to a precompiled dictionary
     in a database somewhere.)
*/
#define TAG_U32_CTRL 0xFFFFFFFF

/* It has the following RPC requests defined as sub-tags: */
#define TAG_U32_CTRL_ID_NAME   0  /* Map identifier to name */
#define TAG_U32_CTRL_ID_TYPE   1  /* Map identifier to type */
#define TAG_U32_CTRL_NAME_ID   2  /* Map name to identifier. */


struct tag_u32_entry {
    const char *name;
    const char *type;
    tag_u32_handle_fn handle;
    uint32_t nb_args; // minimal nb args for substructure
};

int handle_tag_u32_map(struct tag_u32 *r,
                       const struct tag_u32_entry *map, uint32_t nb_entries);

/* Abstract map access. */
typedef int (*map_ref_fn)(struct tag_u32 *r, void *,
                          struct tag_u32_entry *entry);
int handle_tag_u32_map_ref_meta(struct tag_u32 *r,
                                map_ref_fn map_ref, void *ctx);

/* Abstraction around handle_tag_u32_map_ref_meta.  See code comments. */
int handle_tag_u32_map_dynamic(struct tag_u32 *req,
                tag_u32_handle_fn sub,
                map_ref_fn fn, void *ctx);


#define HANDLE_TAG_U32_MAP(r, map) \
    handle_tag_u32_map(r, map, ARRAY_SIZE(map))


#define DEF_TAG_U32_MAP_HANDLE(fun_name, map_name)      \
    int fun_name(struct tag_u32 *req) {                 \
        return HANDLE_TAG_U32_MAP(req, map_name);       \
    }

#define DEF_TAG_U32_CONST_MAP_HANDLE(fun_name, ...)                     \
    const struct tag_u32_entry fun_name##_map[] = { __VA_ARGS__ };      \
    DEF_TAG_U32_MAP_HANDLE(fun_name, fun_name##_map)                    \





#endif
