#include <stdint.h>
#include <stdio.h>
int inc = 0;
int main(int argc, char **argv) {
    fprintf(stdout, "test.c wasm main\n");
    inc = 123;
    return 0;
}
int testfun(int i) {
    return i+inc;
}
