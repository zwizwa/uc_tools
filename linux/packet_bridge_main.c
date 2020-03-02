// Small wrapper to allow packet_bridge.c to be used as a library.
// We don't provide processing here, just proxying.
#include "packet_bridge.h"
int main(int argc, char **argv) {
    return packet_forward_main(argc, argv);
}
