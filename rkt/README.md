Attempt to create a small toolbox for generating C code from Racket,
on top of the uc_tools low level macros (csp.h, sm.h, cproc.h etc...)

Some lessons learned from Lua experiment:

- Compilation to state machine needs 2 passes to implement
  continuations as a combination of C local variables and an array of
  bindings that survive yield points.
  
- Generating straight C wihtout intermediate representation is good
  enough for simple languages like this.
  
- Complexity level is at the boundary of what I can manage with just
  Lua.  Need some more abstraction to make this into a robust tool.
  
