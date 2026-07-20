/* Routines for use in virtual table modules. */

#ifndef SQLITE3_VT_J
#define SQLITE3_VT_H

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


#endif
