#ifndef TRACE_BIN_V2_H
#define TRACE_BIN_V2_H

/* Re-implementation of old trace code using same binary API.
   Each log message is associated to:
   - One send function to be called in firmware
   - One packed struct, to be used in host-side parser code

   Each log message field tag is associated to a specific type (i.e. a
   tag named abc always has the type firmware_trace_abc_t). This
   simplifies bookkeeping when converting between typed<->untyped and
   text<->binary and is not a big constraint to accomodate.
*/


/* Support macros for defining tracer functions and packed structs for
   binary logging.  Requires some information to be defined in the
   context:

   firmware_trace_send  Method that sends a log
   firmware_trace_t     Context infor for _send
   firmware_trace_*_t   Atom types
*/


/* Logging field names and logging type names are kept the same. This
   was done originally to make Lua decoding simpler and is good enough
   in practice. */

/* Derive the logging prefix byte from the packet struct defined by
   FIRMWARE_TRACE_DEF_STRUCT.  The byte is high bit 1 + low 7 bits encoding the */
#define FIRMWARE_TRACE_PREFIX(msg) \
    (0x80 | (sizeof(msg) - 5))

typedef uint8_t  firmware_trace_prefix_t;
typedef uint32_t firmware_trace_timestamp_t;
typedef uint8_t  firmware_trace_tag_t;
struct firmware_trace_hdr {
    firmware_trace_prefix_t    prefix;
    firmware_trace_timestamp_t timestamp;
    firmware_trace_tag_t       tag;
} __attribute__((__packed__));



/* Passed to iteration macro to define struct, wrapper function decl and struct init. */
#define FIRMWARE_TRACE_DEF_STRUCT_FIELD(name) firmware_trace_##name##_t name;
#define FIRMWARE_TRACE_DEF_FUNCTION_ARG(name) ,firmware_trace_##name##_t name
#define FIRMWARE_TRACE_STRUCT_FIELD_INIT(name) .name = name,

/* Each message type now has an assocated function called by the
   firwmare, and a packet struct used by that function's
   implementation and also host-side parser code. */

#define FIRMWARE_TRACE_DEF_STRUCT(type)                                 \
    struct firmware_trace_##type {                                      \
        struct firmware_trace_hdr hdr;                                  \
        firmware_trace_##type##_spec(FIRMWARE_TRACE_DEF_STRUCT_FIELD)   \
    } __attribute__((__packed__));

#define FIRMWARE_TRACE_DEF_FUNCTION(type)                               \
    static inline void firmware_trace_##type(                           \
        firmware_trace_t *s                                             \
        firmware_trace_##type##_spec(FIRMWARE_TRACE_DEF_FUNCTION_ARG)) { \
        if (!firmware_trace_enabled(s, trace_tag_##type)) return;       \
        struct firmware_trace_dmx msg = {                               \
            .hdr = { .prefix = FIRMWARE_TRACE_PREFIX(msg),              \
                     .tag = trace_tag_##type },                         \
            firmware_trace_##type##_spec(FIRMWARE_TRACE_STRUCT_FIELD_INIT) \
        };                                                              \
        firmware_trace_send(s, &msg.hdr);                               \
    }

#define FIRMWARE_TRACE_DEF(type)      \
    FIRMWARE_TRACE_DEF_STRUCT(type)   \
    FIRMWARE_TRACE_DEF_FUNCTION(type) \




static inline void firmware_trace_send(
    firmware_trace_t *s, struct firmware_trace_hdr *msg);

static inline int firmware_trace_enabled(
    firmware_trace_t *s, firmware_trace_tag_t tag);

#endif
