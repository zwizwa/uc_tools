#include <stdint.h>
#include <stdio.h>
int inc = 0;
int main(int argc, char **argv) {
    printf("hello\n");
    inc = 123;
    return 0;
}
int testfun(int i) {
    return i+123;
}
