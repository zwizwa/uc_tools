//#pragma GCC push_options
//#pragma GCC optimize ("align-functions=4")

#define FORTH_OUT_INFO 1
#include "mod_forth.c"

const w lit1[] = { (w)enter, (w)lit, (w)1, (w)w_exit };
const w test[] = { (w)enter, (w)lit1, (w)lit1, (w)add, (w)p, (w)w_exit };


#include "macros.h"

void xt_type(const char *name, w xt) {
    LOG("%p %d %d %d %s\n",
        xt, xt_is_word(xt), xt_is_code(xt), xt_is_token(xt), name);
}
#define XT_TYPE(x) xt_type(#x,((w)x))

int main(void) {
    LOG("%s\n", __FILE__);
    XT_TYPE(test);
    XT_TYPE(enter);
    XT_TYPE(lit1);
    XT_TYPE(lit);
    XT_TYPE(w_exit);
    XT_TYPE(add);
    XT_TYPE(p);
    XT_TYPE(YIELD);
    w xt = { .cpw = &test[0] };
    run(xt);
    return 0;
}

//#pragma GCC pop_options
