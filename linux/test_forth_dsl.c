
#include "mod_forth_dsl_tc.c"
//#include "mod_forth_dsl_cprim.c"
#include "mod_forth_dsl.c"

int main(int argc, char **argv) {
    struct forth_dsl_state s;
    forth_dsl_init(&s);
    uint8_t input[] = {1,2,3};
    forth_dsl_write(&s, input, sizeof(input));
    uint8_t c;
    while (1 == cbuf_read(&s.out, &c, 1)) {
        LOG(" %d\n", c);
    }
    LOG("\n");
    return 0;
}
