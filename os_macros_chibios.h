#ifndef OS_MACROS_CH
#define OS_MACROS_CH

#ifndef LOG
#define LOG_STDERR(...) do {                                            \
        /* semihosting is slow, so buffer it */                         \
        char macros_log_buf[128];                                       \
        snprintf(macros_log_buf, sizeof(macros_log_buf), __VA_ARGS__);  \
        fputs(macros_log_buf, stderr);                                  \
    } while(0)
extern mutex_t info_buf_mutex;
int infof(const char *fmt, ...);
#define LOG_INFOF(...) do {                                             \
        chMtxLock(&info_buf_mutex);                                     \
        infof(__VA_ARGS__);                                             \
        chMtxUnlock(&info_buf_mutex);                                   \
    } while(0)

/* An alternative to semihosting, which is slow and needs to halt the
   target while performing the hosted operations, there is also the
   option to use Black Magic Probe firmware that can read out info_buf
   as part of its polling loop while waiting for the target to halt.
   We use the info log as a default to use either that approach, or
   some custom logging mechanism defined in the firmware. */
#define LOG LOG_INFOF
//#define LOG LOG_STDERR
#endif

#ifndef ABORT
#define ABORT while(1) // FIXME
#endif

#endif
