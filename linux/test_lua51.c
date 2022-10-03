// Include before anything else.  This will implement ABORT in ERROR
// and ASSERT messages via setjmp/longjmp.
#include "mod_abort_longjmp.c"

// See comments in mod file
#include "mod_test_lua51.c"

// This file is just a wrapper, instantiating the mod code.

#include <unistd.h>
static int wait_msec_cmd (lua_State *L) {
    int msec = (int) luaL_checknumber (L, -1);
    lua_pop (L, 1);
    usleep (msec * 1000);
    return 0;
}

/* ... more functions ... */


int luaopen_test_lua51 (lua_State *L) {

    new_lua_metatable(L, pbuf_a_T, pbuf_a_gc);

    /* This table is what is returned by 'require' */
    lua_newtable(L);

    DEF_CFUN(wait_msec);
    DEF_CFUN(pbuf_a_new);

    for_heap_tests(DEF_TCFUN);


    return 1;
}
