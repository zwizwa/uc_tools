#include <stdint.h>
typedef struct {
    uint32_t stack[10];
    uint32_t top;
} test_stack_t;
typedef uint32_t test_element_t;
#define NS(s) test##s
#include "ns_stack.h"

#include "macros.h"

int main(void) {
    test_stack_t s;
    // test_stack_init(&s);
    test_pool_init(&s, ARRAY_SIZE(s.stack));
    LOG("pop:");
    while(test_size(&s)) { LOG( " %d", test_pop(&s)); }
    LOG("\n");
    return 0;
}
