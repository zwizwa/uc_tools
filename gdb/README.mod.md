Compile-time configurable modules.

In the absence of something like C++ templates, a C code base is
typically parameterized with macros to create an opportunity for the C
compiler to remove indirections and generate more compact code.

Since `uc_tools` can compile multiple `.elf` files in a single build
pass, a separate configuration mechanism is not necessary.

- Any template-like code can go into separate `mod_*.c` files,
  parameterized by macros or global functions and variables.

- Configuration symbols (macros, global variables and specialized
  functions) can go in the the firmware's main `.c` file, defined
  before including the `mod_*.c` files.

The reasons to use the `mod_*.c` naming convention is to indicate that
the purpose is not the same as a generic `.h` or `.c` file.  They they
require parameterization before inclusion, and they perform
instantiation of code and data.

The presence of this pattern creates a tradeoff: should code go in a
reusable library, a header file with inline functions, or should it be
a parameterizable module?  These are the rules I try to follow:

- Everything that can be abstracted in static inline functions or
  macros should be placed in `.h` files.  This should be independent
  of hardware, possibly linking to HAL functions.
  
- All code that doesn't need to be inlined should go in `.c` files, be
  included in a main `lib.a` file, and should also be independent of
  hardware, possibly referring to HAL functions.
  
- Anything else goes in `mod_*.c` files.  Typically this is glue code
  that e.g. instantiates inline functions and global data structures
  from `.h` files
