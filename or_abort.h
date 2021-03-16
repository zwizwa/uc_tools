#ifndef OR_ABORT_H
#define OR_ABORT_H

/* Experiment:

   Allow for C code to be written in "emulated Either monad" form
   implementing early exit on error, meaning that it looks like:

   int32_t val1 = read(s);  OR_ABORT(s,0);
   int32_t val2 = read(s);  OR_ABORT(s,0);
   ...

   Emulating Haskell's:

   do
     val1 <- read
     val2 <- read
     ...

   or Rust's:

   let val1 = read(s)?;
   let val2 = read(s)?;

   This isn't a perfect match, but the straightforwardness might be
   worth it.  The main hack is that OR_ABORT can be implemented as a
   single macro that is based only on the assumption that ->error is
   defined and false indicates ok, this way pointers and integers can
   be used as error codes.

   The argument s is the context struct, the argument v is the dummy
   return value that is necessary to keep C happy.  Note that the
   convention is that if s->error is non-zero, the return value is not
   valid.

*/

#define OR_ABORT(s,v) \
    do { if (s->error) return v; } while(0)

#define OR_ABORT_VOID(s) \
    do { if (s->error) return; } while(0)

#endif
