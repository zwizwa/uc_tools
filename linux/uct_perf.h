#ifndef UCT_PERF_H
#define UCT_PERF_H
#include "macros.h"

#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <string.h>
#include <sys/ioctl.h>

static inline long perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
                                   int cpu, int group_fd, unsigned long flags) {
    return syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
}

static inline int uct_perf_open(void) {
    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(pe));
    pe.type = PERF_TYPE_HARDWARE;
    pe.size = sizeof(pe);
    pe.config = PERF_COUNT_HW_CPU_CYCLES;
    pe.disabled = 1;
    pe.exclude_kernel = 1;
    pe.exclude_hv = 1;

    int fd = perf_event_open(&pe, 0, -1, -1, 0);
    if (fd == -1) {
        perror("perf_event_open");
    }
    return fd;
}
static inline void uct_perf_start(int fd) {
    ioctl(fd, PERF_EVENT_IOC_RESET, 0);
    ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
}
static inline long long uct_perf_stop(int fd) {
    ioctl(fd, PERF_EVENT_IOC_DISABLE, 0);
    long long count;
    ASSERT(sizeof(count) == read(fd, &count, sizeof(count)));
    return count;
}
static void uct_perf_close(int fd) {
    close(fd);
}

static inline void uct_perf_test(void) {
    int fd = uct_perf_open();
    ASSERT(fd != -1);
    uct_perf_start(fd);
    long long count = uct_perf_stop(fd);
    LOG("cycles = %lld\n", count);
    uct_perf_close(fd);
}


#endif
