Introduction
------------

If you use C++ then use templates.

This describes two disciplines that implement generic code based on
instantiating "open" C code.

General idea is to

1. eliminate callbacks and dynamic casts such that the C compiler can
   properly type-check everything that is known at compile time, and

2. to allow composition of types and macros so the compiler can
   generate efficient code.


Namespace modules
-----------------

Motivation: implementation of generic algorithms and data structures
that can be specialized / parameterized by functions, types and
macros.

```c
#define NS(name) my_namespace##name
#include "ns_generic.h"
#undef NS
```

The code in `ns_generic.h` then has all name references wrapped in
`NS()`.  This allows the caller (includer) to set up an environment of
functions, types and macros and import behavor from the generic file.

Note that this only goes one deep: it is not possible to include an
`ns_*.h` file from inside another one unless the `NS()` macro is the
same.  However it is flexible enough for most practical purposes in
`uc_tools`.

[Example](../ns_stack.h)




Component modules
-----------------

Motivation: typical deeply embedded code has a component architecture,
i.e. the code is split up into a collection of concrete components
that relate to each other.

The idea is then again to write "open" code, where a component assumes
that another component is available, and interfaces with it via
functions, types and macros defined by that other component.

Several components are then included into a single C file with
appropriate "zero cost" glue consisting of type defintions, (inline)
functions, and macros.

