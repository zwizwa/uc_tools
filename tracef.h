#ifndef TRACEF_H
#define TRACEF_H

#include <stdarg.h>
#include <stdint.h>

int vtracef_text(uint32_t tag_ignored, const char *fmt, va_list ap);
int vtracef_binary(uint32_t tag, const char *fmt, va_list ap);
int tracef(uint32_t tag, const char *fmt, ...);

#define TRACE(fmt, ...) tracef(fmt, #fmt, __VA_ARGS__)

#endif
