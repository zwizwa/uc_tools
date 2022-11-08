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
function update_counter(state)
   local output = state
   local next_state = state + 1
   return next_state, output
end
l.print(update_counter(0))
=> 1, 0
l.print(update_counter(100))
=> 101, 100
```

Given an initial value of type `s`, the update function can be applied
iteratively on each next `s` it produces, yielding an infinite
sequence of `o` as a byproduct.  Let's call this infinite sequence
`Sig o`, a signal of base type o.  One not so useful representation of
this signal is to print its values.

```lua
counter_out={}  -- initial state
counter_state=0
for i=1,3 do
   counter_state, out = update_counter(counter_state)
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
   for i=1,n do
      local head, tail_thunk = unpack(sig)
      l.print(head)
      sig = tail_thunk()
   end
end
print_signal(4, ones)

=> 1
=> 1
=> 1
=> 1
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
counter_signal = as_signal(0, update_counter)
print_signal(4, counter_signal)
=> 0
=> 1
=> 2
=> 3
```


Note that there is no mention of state `s` in `Sig o`.  Similarly in
the `counter_signal` any mention of the state at the "user end" is
gone; it is all tucked away inside the implementation of `as_signal`.

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
print_signal(4, add(counter_signal, ones))
=> 1
=> 2
=> 3
=> 4
print_signal(4, add(counter_signal, counter_signal))
=> 0
=> 2
=> 4
=> 6
```

That seems to work.  Now note that the function `add` only mentions
`+` in one location.  Let's generalize it by wrapping it like this:

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
print_signal(4, add(counter_signal, counter_signal))
=> 0
=> 2
=> 4
=> 6
print_signal(4, sub(counter_signal, ones))
=> -1
=> 0
=> 1
=> 2
print_signal(4, mul(counter_signal, counter_signal))
=> 0
=> 1
=> 4
=> 9
```

Here we have a function `lift_2` that can convert a pure function
operating on signal values to a pure function operating on signals as
a whole.




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



