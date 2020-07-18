#include <stdio.h>
#include <stdint.h>

#include "macros.h"
#include "gensym.h"

typedef struct {
    uint8_t val;
} testq_element_t;

typedef struct {
    int read;
    int write;
    testq_element_t buf[16];
} testq_container_t;

#define NS(name) CONCAT(testq,name)
#include "ns_queue.h"

#define NS(name) CONCAT(testq,name)
#include "ns_queue_iter.h"


int remove_val_pred(void* ctx, const testq_element_t *e) {
    return *((int *)ctx) == e->val;
}
void remove_val(testq_container_t *q, int val) {
    testq_remove(q, remove_val_pred, (void*)&val);
}


int printq_pred(void *ctx, const testq_element_t *e) {
    printf("%2d ", e->val);
    return 1;
}
void printq(testq_container_t *q) {
    printf("(N=%2d) ", testq_nb_stored(q));
    testq_fold(q, printq_pred, NULL);
    printf("\n");
}

const testq_element_t default_e = {.val = 123};
void get_testq_element(testq_element_t const **pe) {
    *pe = &default_e;
}


int main(void) {

    const testq_element_t *e;
    get_testq_element(&e);

    testq_container_t q = {};
    for(int i=0; i<20; i++) {
        testq_element_t e = { .val = i };
        testq_write(&q, &e);
        //printf("%d\n", testq_nb_stored(&q));
    }
    printq(&q);

    testq_cycle(&q);
    printq(&q);

    testq_drop(&q);
    printq(&q);

    remove_val(&q,7);
    printq(&q);

    for(int i=0; i<20; i++) {
        testq_drop(&q);
        //printf("%d\n", testq_nb_stored(&q));
    }
    printq(&q);
}
