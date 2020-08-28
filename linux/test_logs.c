/* How to log distributed systems?

   I'm starting to see a common problem that props up a lot, which is
   how to properly handle ad-hoc time series data such as logs from a
   distributed application, emails, text logs across projects etc.

   The basic problem is that I do not find a good representation,
   which prevents me to come up with decent, practical abstractions.

   Some recent events that lead to this.

   - trying to debug low level bus timing errors in a packet switching
     application implemented on a bare metal uC

   - getting annoyed with thunderbird being so slow indexing a large
     bulk of email.

   - not knowing how to manage a large collection of chronological lab
     note entries spread across a large number of files.  ( same
     problem structure as email? )

   - re-read apenwarr blog post about structured logs.  main
     conclusion being that 1. you want structured logs, and 2. you
     want to be able to construct them from unstructured logs (text
     files).

   A main thread in all this is a way to manage large data sets so
   they can be somehow filtered or simplified, and to also make this
   fast.

   For implementation I'm thinking Rust or C.  I already started doing
   the lab notes parser in Rust.  However it seems that the low level
   "fast" routines would not be that hard to do in C, so let's explore
   that for now.

   This should be written as core iterators aimed to be integrated
   into another framework.  I have two things in mind atm: SQLite3
   virtual tables and integration into Lua for use in an existing test
   system.

   Integration into existing Erlang / exo infrastructure can probably
   be done via SQLite.

   TODO:

   - create iterators for memory-mapped files

   - expose this as a sqlite3 virtual table plugin

   - integrate with logan

   - make a Rust version / wrapper?

*/


#include "log_entry.h"
#include "assert_mmap.h"
#include <stdint.h>


// https://sqlite.org/src/file/ext/misc/csv.c
// https://sqlite.org/c3ref/create_module.html
// https://sqlite.org/vtab.html

void log_header_line(void *ctx, const uint8_t *msg, uintptr_t len) {
    LOG("%p\t  l: %d\n", msg, len);
}

uintptr_t log_entry(void *ctx, const uint8_t *msg, uintptr_t len) {
    LOG("%p\t e: %d\n", msg, len);
    const uint8_t *body = log_entry_for_line(msg, len, log_header_line, 0);
    LOG("%p\t  b: %d\n", body, len - (body - msg));
    return 0; // continue
}

/* This is set up as a test runner. */
int main(int argc, char **argv) {
    ASSERT(argc >= 2);
    if (!strcmp(argv[1], "entry")) {
        ASSERT(argc == 3);
        off_t size = 0;
        const uint8_t *mem = assert_mmap_rdonly(argv[2], 0, &size);
        LOG("%p\t f: %d\n", mem, size);
        uintptr_t hdr_len = log_entry_header(mem, size);
        LOG("%p\t 0: %d\n", mem, hdr_len);
        log_entry_for(mem+hdr_len, size-hdr_len, log_entry, 0);
    }
    return 0;
}


