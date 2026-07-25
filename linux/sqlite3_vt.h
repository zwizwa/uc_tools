/* Routines for use in virtual table modules. */

#ifndef SQLITE3_VT_H
#define SQLITE3_VT_H

#include <sqlite3ext.h>

/* This redirects calls through the sqlite3_api pointer table. */
SQLITE_EXTENSION_INIT1


#include "base64.h"


static inline void db_log_index_info(sqlite3_index_info *p){
    int i;
    LOG(" nConstraint=%d\n", p->nConstraint);
    for(i=0; i<p->nConstraint; i++){
        struct sqlite3_index_constraint *c = &p->aConstraint[i];
        LOG("  constraint[%d]: iColumn=%d op=%d usable=%d%s\n",
            i, c->iColumn, c->op, c->usable,
            c->op==SQLITE_INDEX_CONSTRAINT_EQ ? "  <-- EQ" : "");
    }
    LOG(" nOrderBy=%d\n", p->nOrderBy);
    for(i=0; i<p->nOrderBy; i++){
        LOG("  orderby[%d]: iColumn=%d desc=%d\n",
            i, p->aOrderBy[i].iColumn, p->aOrderBy[i].desc);
    }
    LOG(" nConstraint usage:\n");
    for(i=0; i<p->nConstraint; i++){
        LOG("  usage[%d]: argvIndex=%d omit=%d\n",
            i, p->aConstraintUsage[i].argvIndex, p->aConstraintUsage[i].omit);
    }
}


/* The communication between xBestIndex and xFilter only gets:
   int idxNum,  const char *idxStr,  // Set by xBestIndex
   int argc, sqlite3_value **argv    // Constraint values filled in by sqlite

   So idxNum and idxStr should encode how to interpret the arguments.
   They pass information that is constructed at statement preparation
   time and is shared across multiple invocations.

   The arguments are unique per invocation.

   To pass information from xBestIndex to xFilter, we use base64
   encoding.  There is no generic C data structure + deallocation
   callback for this.

   SQLite handles the deallocation of the idxStr, e.g. discards when
   an xBestIndex result is not used, or stored in the prepared
   statement and freed on finalize.
*/

static inline char *db_idxString_base64_encode(const uint8_t *data, size_t len) {
    size_t n = base64_encoded_length(len);
    char *enc = sqlite3_malloc(n+1);
    base64_encode(enc, data, len);
    return enc;

}
static inline uint8_t *db_idxString_base64_decode(const char *enc,
                                                  size_t len,
                                                  size_t *output_len) {
    size_t n = base64_decoded_length(enc, len);
    uint8_t *data = sqlite3_malloc(n+1);
    base64_decode(data, enc, len, output_len);
    return data;
}


#endif
