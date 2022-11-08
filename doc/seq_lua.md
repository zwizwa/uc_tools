Introduction
------------

This is a literal lua file. This is a literal lua file.  Lines
starting with '=' are program output.

```lua
l = require('literal')
```

Pure Signals and Signal Processors
----------------------------------

Signals and signal processors can be represented as composition of
pure functions.

The following uses Haskell type notation interspersed with some Lua
example code.

Lower case denote type variables.  Upper case denote concrete
types.  E.g. `t` could represent a concrete `Float` or `Int` type.
Function types are denoted as `a->b`, a function that maps values
of type `a` to values of type `b`.  E.g. `Float->Float` is a
function that maps `Float` to `Float`, e.g. the `sin` function is
of that type.  Tuples are used to bundle values of arbitrary types,
with the tuple type denoted as `(a,b)`.  E.g. `(Float,Int)`.

The main idea is that causal signals can be represented by a pure
update function of type `s->(s,o)`, a function that takes a state
value of type `s` and produces the next state value of type `s`
together with an output value of type `o`.

E.g. `Int->(Int,Int)` could represent the type of a counter with an
`Int` state and an `Int` output.

```lua
function update_counter(state)
   local output = state
   local next_state = state + 1
   return next_state, output
end
l.print(update_counter(1))
=>
l.print(update_counter(2))
=>
```

Given an initial value of type `s`, the update function can be applied
iteratively on each next `s` it produces, yielding an infinite
sequence of `o` as a byproduct.  We call this infinite sequence `Sig o`,
a signal of base type o.  The point of `seq.lua` is to provide a
representation of such signals, but for now assume it is possible.

Note that there is no mention of state `s` in `Sig o`.  Once we build
a signal, we do not need to care about the state that is involved in
generating it.

This hints at something: it should be possible to have the programmer
work with just `Sig o`, and not think about state or "processor
instances" at all, performing all the state bookkeeping under the
hood.

So we have signals.  This idea can be extended to update functions of
type `(s,i)->(s,o)` representing a stateful signal processor that
takes input of type `i` together with state `s` to output of type `o`
together with next state `s`.

We can do the following: start with a given signal `Sig i` and an
initial state value for state `s`. Take the first `i` from `Sig i` and
feed it into the update function together with the initial state value
`s`.  Take the `s` that comes out, record the `o`.  Feed the `s` back
in taking the next `i` from `Sig i` etc.  Again, the output is an
infinite sequence of `o` represented as `Sig o`.  Effectively what
we've done here is to create a function `Sig i -> Sig o`, taking an
infinite sequence of `i` and mapping it to an infinite sequence of
`o`.

For most of the discussion, the initial state value can be ignored,
just keep in mind that it is always tracked.  For most applications
the initial state can even be set to `0`.

So we now have signals `Sig a` and signal processors `Sig i -> Sig o`
constructed from update functions.  If we now add any pure function
`i->o` and provide a lifting mechanism (fmap) that produces `Sig i ->
Sig o` functions, we have a language that is about as expressive as
Faust.

Now it turns out that update functions of type `(s,i)->(s,o)` are best
represented as `i->(s->(s,o))`.  This alows the implementation to
focus on the transformation of `s->(s,o)` to `Sig o`.

Then one last thing.  In order to make things composable, the scalar
`s->(s,o)` update functions need to be embedded as `Sig s -> (Sig s,
Sig o)` update functions.  This is the same as long as it is assumed
that the output `Sig s` is the input `Sig a` shifted one step into the
future.

The Lua implementation also adds nested vectors to this, which we'll
ignore for now.  It is a straightforward extension.  The important
point is how to deal with time and state.


Embedded DSLs and Higher order Syntax
-------------------------------------



TODO: examples



