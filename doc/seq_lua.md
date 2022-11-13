Introduction
------------

This is a literal Lua file.  Lines starting with '=>' are program
output.

```lua
l = require('literal')
```

Pure Signals and Signal Processors
----------------------------------

The following uses Haskell type notation interspersed with some Lua
example code.

Lower case denote type variables.  Upper case denote concrete
types.  E.g. `t` could represent a concrete `Float` or `Int` type.
Function types are denoted as `a->b`, a function that maps values
of type `a` to values of type `b`.  E.g. `Float->Float` is a
function that maps `Float` to `Float`, e.g. the `sin` function is
of that type.  Tuples are used to bundle values of arbitrary types,
with the tuple type denoted as `(a,b)`.  E.g. `(Float,Int)`.

We are interested in recursive signal processing using update
functions of type `(s,i)->(s,o)` that take input state and input value
and compute next state and output.  This update function is then
iterated for each sample value in an input signal `Sig i` to produce
output signal `Sig o` while passing the state value `s` between
updates.

The key insight is that expressing update equations in terms of the
infinite signals allows the construction of a recursion operator that
can hide state, allowing the programmer to focus on input/output
behavior without the need to manage stateful objects.

The basic idea here is that a signal `Sig a` is an Applicative
Functor, i.e. we can lift multi-argument functions from the value
domain `a` into the signal domain `Sig a`.

In addition, `Sig` supports a recursion operator
`rec :: (Sig s -> (Sig s, Sig o)) -> Sig o`
that is used to hide state propagation.

In other words, it is simpler to work with the pure functions `Sig i
-> Sig o` directly than to carry around update equations of the form
`(s,i) -> (s,o)`, or work with stateful objects that need explicit
instantiation.

This section shows how to build a model for `Sig` in Lua based on lazy
lists.

The next section will illustrate how this representation can be used
to compose signal processors that can then be compiled back to one
large `(s,i)->(s,o)` update form to be implemented in a low level
language like C or Verilog.




Let's start with a representation of a causal signal that can be
represented by a pure update function of type `s->(s,o)`, a function
that takes a state value of type `s` and produces the next state value
of type `s` together with an output value of type `o`.

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

Now instead of printing out the sequence, represent is as a lazy list. This can
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

Generalize this by wrapping it in a function parameterized by the
signal's constant value.

```lua
function pure(val) return signal(val, function() return pure(val) end) end
zeros = pure(0)
print_signal(5, zeros)
=> 0, 0, 0, 0, 0
```

Similarly a counter signal can be represented by guarding the
recursion inside a thunk.

```lua
function update_to_signal(state, update)
   local next_state, output = update(state)
   return signal(
      output,
      function() return update_to_signal(next_state, update) end)
end
counter_signal = update_to_signal(0, counter_update)
print_signal(5, counter_signal)
=> 0, 1, 2, 3, 4
```


Note that there is no mention of state `s` in `Sig o`.  Similarly in
the `counter_signal` any mention of the state at the "user end" is
gone; it is all tucked away inside the implementation of `update_to_signal`.

This shows that it is possible to have the programmer work with just
`Sig o`, and not think about state or "processor instances" at all,
performing all the state bookkeeping under the hood.

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

Let's go back to the function `update_to_signal`.  This takes an update
function `s->(s,o)` operating on scalar values op type `s` and `o` and
maps it to an infinite signal `Sig o`.

As stated before we do not want to work in both the scalar domain `o`
and the signal domain `Sig o`.  We want to construct a signal that is
recursively defined by only talking about signal types.

So let's build the operator that maps `(Sig s -> (Sig s, Sig o)` to
`Sig o`.

The operator connects the two `Sig s` types together while inserting a
shift to make it implementable, the latter one being one time step
later than the first one.  This way all the individual `s` values in
the shifted signals "line up" with each other, output connected to
next input all connected via the same `s->(s,o)` function, threading
the state through time.

So essentially, the scalar update function can just be lifted into the
signal domain, and then reduced by identifying the output and input
`Sig s` signals as shifted versions of each other.  This operation is
called `rec`, the recursion combinator, and it has type `(Sig s ->
(Sig s, Sig o) ) -> Sig o`, together with the initial value `s` we
will still need.

The principle for writing lazy operators is the same as before: put
evaluations of things-yet-to-be-defined behind thunks, then patch the
values before those thunks are evaluated.

```lua
function rec(state_val, update)
   local state_next_sig, out_sig
   local state_sig = signal(state_val, function() return state_next_sig end)
   state_next_sig, out_sig = update(state_sig)
   return out_sig
end
```

Here's an example using the `add` stream addition operator and the
`ones` stream of `1` values.

```lua
another_counter = rec(0, function(state) return add(state, ones), state end)
print_signal(5, another_counter)
=> 0, 1, 2, 3, 4
```

With construction of recursively defined signals expressed in terms of
signals, signal processors become quite straightforward: we can
directly use a signal that is presented as a function input in an
update equation that is closed by `rec`.  This is quite remarkable if
you think of it for a moment.

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
=> 0, 0, 1, 3, 6
```

That's about it for the basics.

For most of the discussion, especally for audio processors, the
initial state value can typically be ignored and set to `0` by
default.

Aside from the recursive constructor, the `seq.lua` language also
supports nested vectors.  This is fairly straightforward and we'll
ignore it for now.

To wrap up this section, let's add operator overloading and automatic
lifting.

Automatic lifting can be done inside the `lift_2` function.

```lua
function as_signal(thing)
   if type(thing) == 'number' then return pure(thing) end
   return thing
   end
function lift_2(op)
   local sigop
   function sigop(a, b)
      local sig_a = as_signal(a)
      local sig_b = as_signal(b)
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
-- Redefine these with the new lift_2
add = lift_2(function(a,b) return a+b end)
sub = lift_2(function(a,b) return a-b end)
mul = lift_2(function(a,b) return a*b end)
print_signal(5, add(1,1))
=> 2, 2, 2, 2, 2
print_signal(5, add(counter_signal,100))
=> 100, 101, 102, 103, 104
```


Operator overloading can be implemented using a Lua metatatable.

```lua
signal_mt = {__add = add, __sub = sub, __mul = mul }
function signal(head, tail_thunk)
   local sig = {head, memo(tail_thunk)}
   setmetatable(sig, signal_mt)
   return sig
end
print_signal(5, integrate(1) + 200)
=> 200, 201, 202, 203, 204
print_signal(10, integrate(1) * 200)
=> 0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800
```



Embedded DSLs and Higher order Syntax
-------------------------------------

The code above shows that the abstract model is implementable.  Now we
generalize it to be independent of substrate.  Forget for a moment the
`+` operator overloading, we'll add that in later.  An abstract
integrator can be defined like

```lua
function abs_integrator(lang)
   return function(input)
      return lang.rec1(
         0,
         function(state)
            local output = state
            local next_state = lang.add(state, input)
            return next_state, output
         end)
   end
end
```

This defines an integrator in terms of two base language operators
`add` and `rec1`.  The `rec1` is the same as `rec` in the previous
section, just a different naming convention.  Note that we are using
only Lua function abstraction, variable binding and dereference, and
implicitly the Lua argument evaluation order and statement execution
order, but nothing else.

This encoding is called higher order abstract syntax (HOAS).  The
function `abs_integrator` does not have any meaning without the `lang`
parameter.  That parameter is a table containing functions that
implement base langauge constructs.  To evaluate the syntax we can
pass in a `lang_lazylist` object that contains the model we have
implemented.

```lua
lang_lazylist = { rec1 = rec, add = add }
lazylist_integrator = abs_integrator(lang_lazylist)
print_signal(10, lazylist_integrator(ones))
=> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```

We have created an embedded domain specific language (eDSL).  The
language is embedded in Lua, and is domain specific in that it can
only express pure operations on signals together with a recursion
operator that allows the expression of recursively defined signals.




Compiling to C
--------------

In this section we take the abstract syntax `abs_integrator` defined
above, and compile it to C by passing a different `lang`
implementation.

The code below sets up the compiler and uses it to compile the
abstract syntax.

```lua
-- Load the seq language support.
seq = require('lure.seq')
-- C code will be accumulated here to be picked up by document renderer.
c_code = {} 
-- Invoke the compiler, dumping into c_code array.
function compile(prog, nb_args)
   seq.compile1(prog, nb_args, c_code)
end
-- Compile the 1-argument integrator
compile(abs_integrator,1)
```

It produces the following C code.  Note that the outermost `for` loop
is just a wrapper.  The C code generator only supports arrays of
signals, so the input/output/state signals are wrapped in a 1-element
array.

FIXME: The type inference for the input is broken.

```c

/* types:
r5: val
s4: vec(val, 1)
r6: val
v3: vec(val, 1)
i2: idx
*/
void fun(
  /*s*/ val_t s4[1],
  /*i*/ type? a1,
  /*o*/ val_t v3[1]
)
{
  for(idx_t i2 = 0; i2 < 1; i2++) {
    val_t r5 = s4[i2];
    val_t r6 = add(r5, a1);
    s4[i2] = r6;
    v3[i2] = r5;
  }
}
```



Algebra
-------

Let's continue on the idea of substrate independence.  Essentially,
the eDSL is just algebra.  We can use this representation to express
other things such as:

    - Complex Numbers
    
    - Dual Numbers / Automatic differentiation

    - Z Transform

