#include "mod_test_heap.c"

// This file is just a wrapper with some static parameters.
// See also test_quickcheck.lua

int main(int argc, char **argv) {
    //         nb_el mul      mod  log
    //--------------------------------
    heap_test1(10,   23,       71, 1);

    heap_test1(20,   23,       71, 0);
    heap_test1(20,    7,       13, 0);
    heap_test2(20,    7,       13, 0);

    heap_test1(10,    0,       23, 0);
    heap_test1(100,   0,     2323, 0);
    heap_test1(1000,  0,   232323, 0);
    heap_test1(10000, 0, 23232323, 0);

    heap_test3();
    return 0;
}

