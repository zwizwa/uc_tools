#include "macros.h"
#include "uct_byteswap.h"
#include "gensym.h"
#include "testfunc.h"

/* Common TYP header. */
typedef uint8_t  typ_u8_t;
typedef uint16_t typ_u16_t;
typedef uint32_t typ_u32_t;
typedef uint8_t  typ_data_t[8];

#define u8_swap 1
#define u16_swap 1
#define u32_swap 1
#define data_swap 0


#define TYP_STRUCT_FOR(field)                   \
    field(a, u8)                                \
    field(b, u16)                               \
    field(c, u32)                               \
    field(d, data)
#define TYP_STRUCT_FIELD(name, typ) \
    typ_##typ##_t name;
#define TYP_STRUCT_FIELD_SIZE(name, typ) \
    {.len = sizeof(typ_##typ##_t), .swap = CONCAT(typ,_swap) },
struct typ { TYP_STRUCT_FOR(TYP_STRUCT_FIELD) } __attribute__((__packed__));

static inline void typ_byteswap(struct typ *typ) {
    const struct byteswap_field bsf[] =
        { TYP_STRUCT_FOR(TYP_STRUCT_FIELD_SIZE) {} };
    byteswap_fields(bsf, (void*)typ);
}



void test_byteswap(void) {
    struct typ t = {
        .a = 0x01,
        .b = 0x0203,
        .c = 0x04050607,
        .d = {8,9,10,11,12,13,14,15}
    };
    log_hex_n(&t, sizeof(t));
    typ_byteswap(&t);
    log_hex_n(&t, sizeof(t));
    typ_byteswap(&t);
    log_hex_n(&t, sizeof(t));
}

int main(int argc, char **argv) {
    LOG("test_misc.c\n");
    ASSERT(0x1234 == SWAP_U16(0x3412));
    ASSERT(0x12345678 == SWAP_U32(0x78563412));
    test_byteswap();
}
