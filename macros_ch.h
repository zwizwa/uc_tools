#ifndef MACROS_CH
#define MACROS_CH

#ifndef LOG
#define LOG(...) do {                                                   \
        /* semihosting is slow, so buffer it */                         \
        char macros_log_buf[100];                                       \
        snprintf(macros_log_buf, sizeof(macros_log_buf), __VA_ARGS__);  \
        fputs(macros_log_buf, stderr);                                  \
    } while(0)
#endif

#ifndef ABORT
#define ABORT while(1) // FIXME
#endif

#endif
