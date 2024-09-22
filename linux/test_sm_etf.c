/* Test harness for sm_etf that doesn't need to run on a uC */

#include "macros.h"
#include "stm32f103/sm_etf.h"
#include "stm32f103/sm_etf.c"
#include <unistd.h>


uint8_t buf[1024];
struct sm_etf sm_etf = {};

void test(const uint8_t *buf, uint32_t len) {
    for (int i=0; i<len; i++) {
        LOG("%02x ", buf[i]);
    }
    LOG("\n");

    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    LOG("status: %08x\n", status);
}
#define TEST(...) { uint8_t buf[] = {__VA_ARGS__}; test(buf, sizeof(buf)); }


void *cb = NULL;

int run_test(const char *test_ignored) {

    // one binding
    sm_etf_init(&sm_etf, buf, sizeof(buf), cb);
    TEST(131,
         LIST_EXT, 0, 0, 0, 1,
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 101, // key
         SMALL_INTEGER_EXT, 102, // value
         NIL_EXT);

    // a nested binding
    sm_etf_init(&sm_etf, buf, sizeof(buf), cb);
    TEST(131,
         LIST_EXT, 0, 0, 0, 1,
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 101, // key

         LIST_EXT, 0, 0, 0, 1,   // value = dict
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 102, // key
         SMALL_INTEGER_EXT, 103, // value
         NIL_EXT,

         NIL_EXT);

    // a nested binding, spit in 2 packets
    sm_etf_init(&sm_etf, buf, sizeof(buf), cb);
    TEST(131,
         LIST_EXT, 0, 0, 0, 1,
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 101, // key

         LIST_EXT, 0, 0, 0, 1);  // value = dict
    TEST(SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 102, // key
         SMALL_INTEGER_EXT, 103, // value
         NIL_EXT,

         NIL_EXT);


    //sm_etf_init(&sm_etf, buf, sizeof(buf));
    //TEST(45);
    return 0;
}

// f(P),P=open_port({spawn, "/home/tom/exo/deps/uc_tools/test/sm_etf/test.elf"},[]).
// port_command(P,term_to_binary([{1,2}])).
// port_command(P,term_to_binary([{1,[{2,3},{4,5}]}])).

int main(int argc, char **argv) {
    if(argc == 2) return run_test(argv[1]);

  again:
    // Run as erlang port.
    sm_etf_init(&sm_etf, buf, sizeof(buf), cb);
    uint8_t msg[16];
    for(;;) {
        ssize_t len = read(0, msg, sizeof(msg));
        LOG("read: %d\n", len);
        if (len == 0) return 0;
        if (len == -1) { perror("read"); return 1; };
        uint32_t status = sm_etf_write(&sm_etf, msg, len);
        switch(status) {
        case SM_WAITING:
            LOG("waiting\n");
            break;
        case SM_HALTED:
            LOG("again\n");
            goto again;
        default:
            LOG("other %08x\n", status);
            return (int)status;
        }
    }
}

