// This is another message serialization format exposed as a sqlite
// table: ilog has concatenated {packet,4} messages, together with an
// index file recording the offsets of packets.

// See sqlite3_logparse.c for reference on how to implement an sqlite
// virtual table.

// SQL side looks like:

// The extension module file is logparse.so
// sqlite> .load ./ilog

// CREATE VIRTUAL TABLE packets USING ilog("packets.ilog");


// Now there are some caveats on making this generic.  At the most
// generic level we can only assume:
// 4-byte BE size tag (for message framing)
// 2-byte uc_tools type tag (for outer-level dispatch)
//
// To further refine there are two paths:
// - Create accessor functions
// - Expose columns, which makes it possible to create indexes
//
// I think I want to work towards the latter approach, but that means
// that different ilog formats will need different vtable objects, so
// most of the code here should probably be put in a "mod", with only
// the column and index access specialized to each.
//
// Actually what claude suggests is to create a single virtual table
// and make the layout decision at load time, i.e. in xConnect.  But
// that won't work if I can't centralize the C code that knows the
// packet types, so let's stick to the mod setup.
//
// https://claude.ai/chat/6c19049a-04d1-40d6-bdbd-fcd7bdb0287e

#include "mod_sqlite3_ilog.c"

static void declare_vtab(sqlite3 *db) {
    int rv = sqlite3_declare_vtab(
        db,
        "CREATE TABLE x("
        "  tag    INTEGER,"
        "  bin    BLOB,"
        "  schema HIDDEN"
        ")");
    ASSERT(rv == SQLITE_OK);
}

static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N) {
    // LOG("xColumn %d\n", N);
    struct ilog_cursor *cur = ilog_cursor(pCur);

    // Perform consitency check and cache the pointer, len.
    get_message(cur);

    switch(N) {
    case 0: {
        uint16_t tag = read_be(cur->msg, 2);
        sqlite3_result_int(c, tag);
        break;
    }
    case 1: {
        // SQLITE_STATIC means the pointers are stable so sqlite will
        // not copy the data.  This works as long as the file is
        // mapped, which should be the case always.
        sqlite3_result_blob(c, cur->msg+4+2, cur->len-2, SQLITE_STATIC);
        break;
    }
    default:
        // Not reached
        SQLITE_ERROR;
    }
    return SQLITE_OK;
}


