#include "sm_csp.h"
#include <stdint.h>


struct sm_client {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    int req;
    int resp;
    int chan;
};
uint32_t client_tick(struct sm_client *s) {
    SM_RESUME(s);
    CSP_RPC(&s->task, s, s->chan, s->req, s->resp);
    CSP_RPC(&s->task, s, s->chan, s->req, s->resp);
    SM_HALT(s);
}

void client_init(struct sm_client *s, int chan) {
    memset(s,0,sizeof(*s));
    s->chan = chan;
}



struct server {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    int req;
    int resp;
    int chan;
};
csp_status_t server_tick(struct server *s) {
    SM_RESUME(s);
    for(;;) {
        CSP_RCV(&s->task, s, s->chan, s->req);
        s->resp = s->req+1;
        CSP_SND(&s->task, s, s->chan, s->resp);
    }
}
void server_init(struct server *s, int chan) {
    memset(s,0,sizeof(*s));
    s->chan = chan;
}



void test1(struct csp_scheduler *s) {
    struct server m;
    struct sm_client t;
    int chan = 0;
    SM_CSP_START(s, &m.task,  server, &m, chan);
    SM_CSP_START(s, &t.task,  client, &t, chan);
    csp_schedule(s);
    LOG("\n");
}
int main(int argc, char **argv) {
    int nb_c2e = 30;
    int nb_c = 30;
    csp_with_scheduler(nb_c2e, nb_c, test1);
    return 0;
}
