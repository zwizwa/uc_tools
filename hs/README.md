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

