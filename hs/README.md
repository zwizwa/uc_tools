I keep loosing track of this idea, so here's another attempt:

The problem is that while C + Lua (+ Javascript + Verilog + ASM) is
enough to build a system, the resulting structure is often too hard to
debug and refactor.

What I want is a way to elevate the abstraction level and glue things
together for the testing layer, and possibly allow for some code
generation to be able to use the "commutating compile eval diagram"
where base code is written in Haskell DSLs with Eval and Comp
instances, where Comp is compiled to Lua, C, ASM, Verilog, Javascript
and then compiled further down to binary, then integrated into a
system.  That gives one leg of the diamond.  The same source code is
then composed at the Haskell Eval level as well, which will guarantee
that the types are correct, and the emulation can be instrumented at a
very high level.  It is then possible to split this at arbitrary
levels, where some Haskell emulator wrapper is written for some
compiled code.

A lot of flexibility but the main idea is to keep specification and
testing at the highest possible level.

Summary:

- I am stuck to these lower level language platforms where I really
  miss the lack of types that can check composition at compile time.
  
- I don't want to push Haskell into the face of people that will have
  to use my code later.  So I want to use it as a generator (producing
  editable or "salvageable" code), and a test framework that can be
  discarded.


The code below talks about Lure as the bridge language and using
Racket at the top level language layer.  I no longer thing that is the
correct approach.  I think it really should be Haskell.


Older:

Always the question where to put things / how to organize without
things getting messy over time.  For now the main drive here is c8
test code.

- Lua is the main "people interface" for everything I do.  It's the
  one language that has survived all wars.

- Scheme is the basic model of what I want a simple language to be.
  It also has a lot of flexibility in representing different kinds of
  semantics.  Here Lure is such a reduced semantics, that of Lua.
  
- I want a bridge between Lure and Racket

- I want a bridge between Lure and Haskell

The latter 2 bridges are for writing "phantom type" libraries that
follow this idea:

  Create a statically typed DSL in Haskell that can reduce to
  dynamically typed Lure or Lua.  This is for writing libraries that
  contain data-heavy abstractions that benefit from being written in a
  typed language.
  
The same but in reduced fashion is possible in Racket.

