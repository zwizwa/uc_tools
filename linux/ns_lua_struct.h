/* A ns_*.h style parameterized wrapper for wrapping simple C structs
   as Lua objects. */

/* Userdata wrapper */
struct NS(_ud) {
    struct NS() base;
    /* Most practical code will need to refer back to the Lua context
       from a callback, so add this for all wrappers. */
    lua_State *L;
};
/* Push initialized object to Lua stack, to be used from C code. */
static struct NS(_ud) *NS(_push)(lua_State *L) {
    struct NS(_ud) *ud = lua_newuserdata(L, sizeof(*ud));
    ud->L = L;
    ASSERT(ud);
    NS(_init)(&ud->base);
    luaL_getmetatable(L, NS(_T));
    lua_setmetatable(L, -2);
    return ud;
}
/* Lua command wrapper for the above. */
static int NS(_new_cmd)(lua_State *L) {
    struct NS(_ud) *ud = NS(_push)(L);
    (void)ud;
    return 1;
}
/* Type check */
static struct NS(_ud) *NS(_L)(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, NS(_T)));
    struct NS(_ud) *ud = lua_touserdata(L, index);
    // This started to trigger. WTF?
    // ASSERT(ud->L == L);
    if (ud->L != L) {
        LOG("WARNING: L was %p, is now %p\n", ud->L, L);
        ud->L = L;
    }
    return ud;
}
/* Dummy garbage collect function. */
static int NS(_gc)(lua_State *L) {
    struct NS(_ud) *ud = NS(_L)(L, -1);
    ASSERT(ud->L == L);
    (void)ud;
    return 0;
}
/* Access lua context. */
static inline lua_State *NS(_lua_State)(struct NS() *obj) {
    /* Assume all objects are wrapped. */
    struct NS(_ud) *ud = (void*)obj;
    return ud->L;
}
