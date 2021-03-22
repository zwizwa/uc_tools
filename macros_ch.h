#ifndef MACROS_CH
#define MACROS_CH

#ifndef LOG
#define LOG_STDERR(...) do {                                            \
        /* semihosting is slow, so buffer it */                         \
        char macros_log_buf[100];                                       \
        snprintf(macros_log_buf, sizeof(macros_log_buf), __VA_ARGS__);  \
        fputs(macros_log_buf, stderr);                                  \
    } while(0)
extern mutex_t info_buf_mutex;
int infof(const char *fmt, ...);
#define LOG_INFO(...) do {                                              \
        chMtxLock(&info_buf_mutex);                                     \
        infof(__VA_ARGS__);                                             \
        chMtxUnlock(&info_buf_mutex);                                   \
    } while(0)
// #define LOG LOG_INFO
#define LOG LOG_STDERR
#endif

#ifndef ABORT
#define ABORT while(1) // FIXME
#endif

#endif
