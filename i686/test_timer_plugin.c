/* Test plugin for kernel_timer_at and the main loop -> isr command
   queue, see plugin.h.  A state machine driven by kernel events.
   - ttest:  10 alarms 50ms apart, checks order and lateness
   - tflood: 200 alarms 1ms apart, more than the command queue and
     the alarm table hold: posts until full, continues on
     KERNEL_EVENT_ROOM, checks all fire in order
   make test_timer_plugin.bin
   test_timer.sh */

#include "plugin.h"
#define LOG(...) kernel_infof(__VA_ARGS__)
#include "tools.h"
#include <string.h>

#define TTEST_N  10
#define TFLOOD_N 200

static struct {
    uint32_t n;       // alarms in this run
    uint32_t posted;
    uint32_t fired;
    uint32_t t0;
    uint32_t step;    // ms between alarms
    uint32_t rooms;   // KERNEL_EVENT_ROOM seen
    uint32_t bad;     // out of order or late
    uint32_t max_late;
} s;

static void post_more(void) {
    while (s.posted < s.n) {
        uint32_t at = s.t0 + s.step * (s.posted + 1);
        if (kernel_timer_at(at, s.posted)) return;  // full, wait for ROOM
        s.posted++;
    }
}
static void start(uint32_t n, uint32_t step) {
    memset(&s, 0, sizeof(s));
    s.n = n;
    s.step = step;
    s.t0 = kernel_ms;
    post_more();
    LOG("timer test n=%d posted=%d\n", s.n, s.posted);
}
static void fired(const struct kernel_timer_event *e) {
    uint32_t at = s.t0 + s.step * (e->cookie + 1);
    uint32_t late = e->ms - at;
    if (late > s.max_late) s.max_late = late;
    if (e->cookie != s.fired || late > 2) s.bad++;
    if (++s.fired == s.n) {
        LOG("timer test %s n=%d rooms=%d max_late=%d\n",
            s.bad ? "FAIL" : "ok", s.n, s.rooms, s.max_late);
    }
}

static int word_is(const uint8_t *line, uint32_t len, const char *word) {
    uint32_t n = mini_strlen(word);
    return len > n && !memcmp(line, word, n) && line[n] <= ' ';
}
static int timer_event(uint32_t tag, const uint8_t *data, uint32_t len) {
    switch (tag) {
    case KERNEL_EVENT_LINE:
        if      (word_is(data, len, "ttest"))  start(TTEST_N, 50);
        else if (word_is(data, len, "tflood")) start(TFLOOD_N, 1);
        else return -1;
        return 0;
    case KERNEL_EVENT_TIMER:
        fired((const struct kernel_timer_event *)data);
        return 0;
    case KERNEL_EVENT_ROOM:
        s.rooms++;
        post_more();
        return 0;
    }
    return -1;
}
static struct plugin timer_plugin = {
    .name   = "test_timer",
    .events = (1 << KERNEL_EVENT_LINE) | (1 << KERNEL_EVENT_TIMER) |
              (1 << KERNEL_EVENT_ROOM),
    .event  = timer_event,
};
static struct plugin *timer_init(void) {
    return &timer_plugin;
}
extern uint8_t __plugin_bss_start[], __plugin_bss_end[];
__attribute__((section(".plugin_header")))
const struct plugin_header plugin_header = {
    .magic          = PLUGIN_MAGIC,
    .kernel_version = KERNEL_VERSION,
    .bss_start      = __plugin_bss_start,
    .bss_end        = __plugin_bss_end,
    .init           = timer_init,
};
