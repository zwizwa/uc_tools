//#pragma GCC push_options
//#pragma GCC optimize ("align-functions=4")

#define FORTH_OUT_INFO 1

#include "forth.h"
#include <stdio.h>


#include "mod_forth.c"

const w lit1[] = { (w)enter, (w)lit, (w)1, (w)w_dup, (w)p, (w)w_exit };
const w test[] = { (w)enter, (w)lit1, (w)lit1, (w)add, (w)p, (w)w_exit };

#include "macros.h"

void xt_type(const char *name, w xt) {
    LOG("%p %d %d %d %s\n",
        xt, xt_is_word(xt), xt_is_code(xt), xt_is_token(xt), name);
}
#define XT_TYPE(x) xt_type(#x,((w)x))


void run_test(void) {
    LOG("%s\n", __FILE__);
    XT_TYPE(test);
    XT_TYPE(enter);
    XT_TYPE(lit1);
    XT_TYPE(lit);
    XT_TYPE(w_exit);
    XT_TYPE(add);
    XT_TYPE(p);
    XT_TYPE(w_dup);
    XT_TYPE(YIELD);
    w xt = { .cpw = &test[0] };
    run(xt);
}
int main(int argc, char **argv) {
    forth_start();
    if (argc > 1) {
        if ((argc == 2) && (!strcmp("interactive", argv[1]))) {
            for(;;) {
                int c = getchar();
                if (EOF == c) break;
                uint8_t b = c;
                forth_write(&b, 1);
            }
        }
        else {
            argc--;
            argv++;
            while(argc--) {
                forth_write_word(*argv++);
            }
        }
    }
    else {
        run_test();
    }
    return 0;
}

//#pragma GCC pop_options
