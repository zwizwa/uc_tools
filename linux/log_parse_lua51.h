#ifndef LOG_PARSE_LUA51_H
#define LOG_PARSE_LUA51_H

/* See comments in log_parse_lua51.c */

/* Main reason to expose thes header file is to allow development of
   other Lua C modules that perform custom scanning / unpacking, while
   re-using the main infrastructure.  That has not yet been tested. */

// FIXME: For some reason this (which is already in os_linux.h) is not
// enough to expose mremap.
// #define _GNU_SOURCE
// Workaround:
#define __USE_GNU
#include <sys/mman.h>

#include "log_parse_lua51.h"
#include "mmap_file.h"

// Lua headers are included in this file.
#include "lua_tools.h"
#include "log_parse.h"

/* Memory-mapped file wrapper. */
struct log_file_ud {
    struct mmap_file file;
    lua_State *L;
};

/* Parser iterator wrapper. */
struct log_parse_ud {

    /* Low level iterator. */
    struct log_parse s;

    /* All callbacks are parameterized by the iteration mode, passed
       to low level iterator: LOG_PARSE_STATUS_CONTINUE | YIELD */
    log_parse_status_t mode;

    /* Current offset into the bound file. */
    uintptr_t offset;

    /* For searching. */
    uint8_t prefix;

    /* These are only valid during the execution of a Lua function. */
    lua_State *L;
    uint32_t nb_rv;
    struct log_file_ud *ud_file;

};

#endif
