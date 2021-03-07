#include "mod_forth.c"

const w lit1[] = { (w)enter, (w)lit, (w)1, (w)w_exit };
const w test[] = { (w)enter, (w)lit1, (w)lit1, (w)add, (w)p, (w)w_exit };


#include "macros.h"
int main(void) {
    LOG("%s\n", __FILE__);
    return 0;
}
