

/* FIXME: Review this.

   This is a new pattern, necessary to parameterize the body of a
   function against some macros.  Currently I have to move fast to get
   something going, but clean this up a bit!

   The main reason to put it in a separate file is that the code is
   too long to move it into a macro, so use #include to inline the
   code.

   It's not clear if this is the best way to do it.  Alternatively
   write it as an sm.h style state machine, and just inline that
   machine for blocking operation.
*/

/* This uses the following context:
   S is a pointer to hw_i2c_transmit struct
   c is hw config
*/

