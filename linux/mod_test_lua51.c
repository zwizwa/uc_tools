/* Main idea of this module is to provide access to C application and
   test code from Lua.  This is structurd as a mod_ to make it easier
   to include into a monolith. */

#include "lua_tools.h"

#include "pbuf.h"
#define pbuf_a_T "rdm.pbuf_a"

/* Alternative wrapper for pbufs not using pool. */
struct pbuf_a {
    struct pbuf p;
    uint8_t buf[512+4];
};
void pbuf_a_init(struct pbuf_a *a) {
    pbuf_init(&a->p, a->buf, sizeof(a->buf));
}
#define NS(name) CONCAT(pbuf_a,name)
#include "ns_lua_struct.h"
#undef NS


/* Parameterized tests for heap data structure (software timer) */
#define TEST_LOG(...)
#include "mod_test_heap.c"

static int fun_wrap_i_iiii(struct lua_State *L,
                          int (*fun)(int,int,int,int)) {
    int rv = fun(L_number(L,1),
                 L_number(L,2),
                 L_number(L,3),
                 L_number(L,4));
    lua_pushnumber(L, rv);
    return 1;
}


#define for_heap_tests(m)                        \
    m(i_iiii, heap_test1)                         \



for_heap_tests(DEF_FUN_CMD)



/* Wrapper for num_heap defined in mod_test_heap.c */
#define num_heap_a_T "rdm.num_heap_a"
struct num_heap_a {
    struct num_heap heap;
    uint32_t arr[512];
};
void num_heap_a_init(struct num_heap_a *a) {
    a->heap.nb = 0;
    a->heap.arr = a->arr;
}
#define NS(name) CONCAT(num_heap_a,name)
#include "ns_lua_struct.h"
#undef NS

static int fun_wrap_v_hu(struct lua_State *L,
                         void (*fun)(struct num_heap *h, uint32_t)) {
    fun(&num_heap_a_L(L, 1)->base.heap, L_number(L,2));
    return 0;
}
static int fun_wrap_u_h(struct lua_State *L,
                        uint32_t (*fun)(struct num_heap *h)) {
    lua_pushnumber(L, fun(&num_heap_a_L(L, 1)->base.heap));
    return 1;
}
static int fun_wrap_w_h(struct lua_State *L,
                        uintptr_t (*fun)(struct num_heap *h)) {
    lua_pushnumber(L, fun(&num_heap_a_L(L, 1)->base.heap));
    return 1;
}

#define for_num_heap(m)                    \
    m(v_hu, num_heap_insert)               \
    m(u_h,  num_heap_peek)                 \
    m(u_h,  num_heap_pop)                  \
    m(w_h,  num_heap_nb)                   \

for_num_heap(DEF_FUN_CMD)
