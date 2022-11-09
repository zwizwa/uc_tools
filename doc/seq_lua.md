Introduction
------------

This is a literal lua file.  Lines starting with '=>' are program
output.

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
function counter_update(state)
   local output = state
   local next_state = state + 1
   return next_state, output
end
l.print(counter_update(0))
=> 1, 0
l.print(counter_update(100))
=> 101, 100
```

Given an initial value of type `s`, the update function can be applied
iteratively on each next `s` it produces, yielding an infinite
sequence of `o` as a byproduct.  Let's call this infinite sequence
`Sig o`, a signal of base type o.  One not so useful representation of
this signal is to print its values.

```lua
counter_state=0
for i=1,3 do
   counter_state, out = counter_update(counter_state)
   l.print(out)
end
=> 0
=> 1
=> 2
```

But we can do much more.  The point of `seq.lua` is to provide an
abstract representation of such signals that can be used to generate C
code.  But we need to introduce a couple of ideas first, so let's work
with just Lua code first.

One way to represent a signal in Lua is by using a lazy list. This can
be built from a memoization function that caches the evaluation of a
zero argument function (a thunk).
```lua
function memo(thunk)
   local cache = nil
   return function()
      if cache ~= nil then return cache end
      cache = thunk()
      return cache
   end
end
```

This memoization function is then used to build a list as a pair of a
head element, and a thunk that evaluates to the tail.


```lua
function signal(head, tail_thunk)
   return {head, memo(tail_thunk)}
end
```

An infinite sequence of `1` could be built as
```lua
ones = signal(1, function() return ones end)
```

To print it, iteratively print the head then evaluate the tail.
```lua
function print_signal(n, sig)
   local chunk = {}
   for i=1,n do
      local head, tail_thunk = unpack(sig)
      chunk[i] = head
      sig = tail_thunk()
   end
   l.print(unpack(chunk))
end
print_signal(5, ones)

=> 1, 1, 1, 1, 1
```

Finally, the counter signal can be represented by guarding the
recursion inside a thunk.

```lua
function as_signal(state, update)
   local next_state, output = update(state)
   return signal(
      output,
      function() return as_signal(next_state, update) end)
end
counter_signal = as_signal(0, counter_update)
print_signal(5, counter_signal)
=> 0, 1, 2, 3, 4
```


Note that there is no mention of state `s` in `Sig o`.  Similarly in
the `counter_signal` any mention of the state at the "user end" is
gone; it is all tucked away inside the implementation of `as_signal`.

This hints at something: it should be possible to have the programmer
work with just `Sig o`, and not think about state or "processor
instances" at all, performing all the state bookkeeping under the
hood.

So what can we do with these signals.  Well, let's try to add them.

```lua
function add(sig_a, sig_b)
   local head_a, tail_thunk_a = unpack(sig_a)
   local head_b, tail_thunk_b = unpack(sig_b)
   return signal(
      head_a + head_b,
      function()
         return add(tail_thunk_a(), tail_thunk_b())
      end)
end
print_signal(5, add(counter_signal, ones))
=> 1, 2, 3, 4, 5
print_signal(5, add(counter_signal, counter_signal))
=> 0, 2, 4, 6, 8
```

Now note that the function `add` only mentions `+` in one location.
Let's generalize it by wrapping it like this:

```lua
function lift_2(op)
   local sigop
   function sigop(sig_a, sig_b)
      local head_a, tail_thunk_a = unpack(sig_a)
      local head_b, tail_thunk_b = unpack(sig_b)
      return signal(
         op(head_a, head_b),
         function()
            return sigop(tail_thunk_a(), tail_thunk_b())
         end)
   end
   return sigop
end
add = lift_2(function(a,b) return a + b end)
sub = lift_2(function(a,b) return a - b end)
mul = lift_2(function(a,b) return a * b end)
print_signal(5, add(counter_signal, counter_signal))
=> 0, 2, 4, 6, 8
print_signal(5, sub(counter_signal, ones))
=> -1, 0, 1, 2, 3
print_signal(5, mul(counter_signal, counter_signal))
=> 0, 1, 4, 9, 16
```

Here we have a function `lift_2` that can convert a pure function
operating on signal values to a pure function operating on signals as
a whole.

Let's go back to the function `as_signal`.  This takes an update
function `s->(s,o)` operating on scalar values op type `s` and `o` and
maps it to an infinite signal `Sig o`.

For reasons that will become clear later on, we do not want to work in
both `o` and `Sig o` domains: the scalar domain and the signal domain.
We want to construct a signal that is recursively defined by only
talking about signal types.

E.g. given a function of type `Sig s -> (Sig s, Sig o)`, construct a
signal of type `Sig o`.  Does this even make sense?

Drumroll... yes!

If the two `Sig s` values are thought of as shifted versions of each
other, the latter one being one time step later than the first one,
all the individual `s` values in the signals "line up" with each
other, all connected via the same `s->(s,o)` function.

So essentially, the scalar update function can just be lifted into the
signal domain, and then reduced by identifying the output and input
`Sig s` signals as shifted versions of each other.  This operation is
called `rec`, the recursion combinator, and it has type `(Sig s ->
(Sig s, Sig o) ) -> Sig o`, together with the initial value `s` we
will still need.

Implementation of lazy operators in a strict language can be tricky,
but the basic idea is to declare names, then patch values before they
are evaluated.

```lua
function rec(state_val, update)
   local state_tail, out_sig
   local function out_sig_tail() return rec(state_tail[1], update) end
   state_tail, out_sig = update(signal(state_val, state_tail))
   return signal(out_sig[1], out_sig_tail)
end
```

Here's an example using the `add` stream addition operator and the
`ones` stream of `1` values.

```lua
another_counter = rec(0, function(state) return add(state, ones), state end)
print_signal(5, another_counter)
=> 0, 1, 2, 3, 4
```

This then paves the way to encode signal processors as well:

```lua
function integrate(input)
   return rec(
      0,
      function(state)
         return add(state, input), state
      end)
end
print_signal(5, integrate(ones))
=> 0, 1, 2, 3, 4
print_signal(5, integrate(counter_signal))
-- FIXME: This is not correct
=> 0, 0, 0, 0, 0
```






OLD

we can indeed build an operator




So what else do we need?  What about building a filter?  Something
that looks nice with whole numbers, e.g. an integrator.  Here is where
it gets interesting and a little confusing.

What we want is the signal version of an update functions of type
`(s,i)->(s,o)` representing a stateful signal processor that takes
input of type `i` together with state `s` to output of type `o`
together with next state `s`.

Now here comes the leap of fate: instead of the type above, let's
focus on the curried variant `i->(s->(s,o))`.  


```lua
function integrate_update(state, input)
   local next_state = state + input
   local out = state
   return next_state, output
end
```

To turn this into a function of type `Sig i -> Sig o` we can do
something like.

```lua

```



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



