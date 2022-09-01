Grab bag of tools for real-time microcontroller applications. 

This code supports development of bare metal firmware, i.e. finite
state machines on low-memory microcontrollers, optimized for efficient
hardware binding and zero-cost abstraction.

A note on style: most of the abstractions in the C code in this
archive are optimized for efficient instantiation, meaning that the
code that goes on the controller is written such that the compiler can
optimize very aggressively.  Do not use this library if you need to
disable optimization for certification purposes, as the code will be
very inefficient.

The code that is compile-time only, i.e. the C macro code, does use a
lot of tricks that might need a bit more context to understand
properly.  They are heavily rooted in functional programming, staged
macros and compiler lore, translated to constructs that can be
expressed using the C preprocessor.  It took a while to find a good
set of abstractions, but I am very happy about where this ended up
eventually.

And as you might have guessed, I do not like C++.  While the C
preprocessor isn't a great tool either, at least it is simple, and
with a bit of discipline it can be used to implement many language
extensions that are based on some decent theory and proven code
generation and language design patterns.  This probably deserves its
own book.


The following are nontrivial abstractions that have some thought
behind them, and probably deserve their own documentation:

- sm.h : A macro langauge for writing composable sequential state
  machines.  Think "tasks with static stack frames".  Backronyms:
  state machine, sub-machine.

- ns_*.h: implementations of generic types (list, queue, heap,
  key_list, serialization) as C namespaces.  Roughly based on ideas
  behind the ML module system (instantiation through explicit type
  specialization).  Similar to C++ templates / STL.

- mod_*.c: a code organization pattern optimized for modular
  compile-time configuration, supporting many specialzed firmware
  images in a single build.

- A set of protocols for interacting with applications running in the
  microcontroller.  Based on Erlang's {packet,N} and SLIP.
  
- Packet Bridge: a (Linux) host tool to convert between different
  packet framing methods (SLIP, Erlang packet format, HEX, raw file
  descriptor, UDP, TAP), and Packet Loop: a {packet,4} based event
  loop.
  
- A CSP scheduler (towards a CSP-based OS)

- A bootloader / monitor / state machine OS based on GDB RSP (GDBSTUB)
  to provide incremental development.  Think "C REPL on a
  micocontroller".

- A small Forth interpreter + host system

- Lua wrappers for elfutils (e.g. inspect target data structures for
  testing pre/post conditions on embedded target).
  
- Small webserver for bootstrapping a websocket connection to embedded
  target.
  
- smc.lua: Preliminary Scheme to protothread compiler


Additionally, there is a collection of mundane tools and applications:
  
- Misc tools: Circular buffers, packet buffers, list, associative
  list, queue, logarithm, pseudorandom numbres, byte swapping, bit
  buffer, crc.

- Audio-related tools: sample player, synth controller code (will be moved)

- Relay board controller

- DHT11 / DHT22 temperature sensor interface

- MDIO interface

- SDIO driver

- Preliminary Ethernet and UDP support

- hw_*.c: Custom high-performance routines for STM32F103, optimized
  from generic OpenOCD code
  
- Preliminary no_std Rust code



See also the erl_tools project which contains support code for
integrating uC applications in a distributed Erlang network, and has
support for the protocol used in the bootloader.
  
See also the Staapl project, which contains some seed ideas for
techniques used here, but uses a custom Forth dialect instead of C.

