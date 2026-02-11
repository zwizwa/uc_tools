#include "worksteal.h"
int main(int argc, char **argv) {
    LOG("wss_task %d\n", sizeof(struct wss_task));
    struct wss_scheduler wss;
    wss_init(&wss);
    return 0;
}
