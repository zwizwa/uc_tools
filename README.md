Grab bag of tools for real-time microcontroller applications. 

This code supports development of bare metal firmware, i.e. finite
state machines on a small microcontroller, optimized for efficient
hardware binding and low memory usage.

The repository contains a couple of nontrivial abstractions that have
some thought behind them, and probably deserve their own
documentation:

- A bootloader / monitor / state machine OS based on GDB RSP (GDBSTUB)
  to provide incremental development.  Think "C REPL on a
  micocontroller".

- sm.h : A macro langauge for writing composable sequential state
  machines.  Think "tasks with static stack frames".  Backronyms:
  state machine, sub-machine.

- A set of protocols for interacting with applications running in the
  microcontroller.  Based on Erlang's {packet,N} and SLIP.
  
- Packet Bridge: a (Linux) host tool to convert between different
  packet framing methods (SLIP, Erlang packet format, HEX, raw file
  descriptor, UDP, TAP), and Packet Loop: a {packet,4} based event
  loop.
  
- A CSP scheduler (towards a CSP-based OS)

- ns_*.h: implementations of container types (list, queue, heap,
  key_list, serialization) as C namespaces.  Roughly based on ideas
  behind the ML module system.  Similar to C++ templates / STL.

- mod_*.c: a code organization pattern optimized for modular
  compile-time configuration, supporting many specialzed firmware
  images in a single build.

- A small Forth interpreter


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
integrating uC applications in a distributed Erlang network,
  
See also the Staapl project, which contains some seed ideas for
techniques used here, but using a custom Forth dialect instead of C.



