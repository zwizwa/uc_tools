#include <stdlib.h>
#include <stdio.h>

#define LOG printf
int main(int argc, char **argv) {
    LOG("test.c %p\n", argv);
    for (int i=0; i<argc; i++) {
        LOG("- %s\n", argv[i]);
    }
}
