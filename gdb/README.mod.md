Compile-time configurable modules.
==================================

Objective is to allow:

- static configuration using macros, abstracting over init code

- building multiple images/configurations in a single build

In the absence of something like C++ templates, a C code base is
typically parameterized as a while using macros.  This creates an
opportunity for the C compiler to remove indirections and generate
more compact code.

However, the `uc_tools` build system is explicitly designed to be able
to build multiple firmware `.elf` images in a single pass, so we
cannot use a global configuration file.

Therefore the following convention is used:

- Any template-like code (object declarations, init functions) can go
  into separate `mod_*.c` files, parameterized by macros or global
  functions and variables.

- Configuration symbols (macros, global variables and specialized
  functions) can go in the the firmware's main `.c` file, defined
  before including the `mod_*.c` files.

The reasons to use the `mod_*.c` naming convention is to indicate that
such a file has a different purpose than generic `.h` or `.c` file,
which typically contains more abstract code.  The main difference is
that `mod_*.c` files require parameterization before inclusion, and
perform instantiation of code and data.


The presence of this pattern creates the need to choose between two
implementation strategies: should code go in a reusable library, a
header file with inline functions, or should it be a parameterizable
module?  These are the rules I try to follow:

- Everything that can be abstracted in static inline functions or
  macros should be placed in `.h` files.  This should be independent
  of hardware, possibly linking to HAL functions.
  
- All code that doesn't need to be inlined should go in `.c` files, be
  included in a main `lib.a` file, and should also be independent of
  hardware, possibly referring to HAL functions.
  
- Anything else goes in `mod_*.c` files.  Typically this is glue code
  that e.g. instantiates inline functions and global data structures
  from `.h` files


The `instance.h` init system can be used to properly sequence module
initialization at boot.


