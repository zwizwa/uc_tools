Grab bag of tools for real-time microcontroller applications. 

This code supports development of naked state machines running on bare
metal (no RTOS) without dyamic memory allocation.  I.e. malloc is not
linked into the target binary. It contains:

- sm.h : A macro langauge for writing composable sequential state
  machines.  Think "tasks with static stack frames".

- A bootloader / monitor / state machine OS based on GDB RSP (GDBSTUB)
  to provide incremental development.  Think "C REPL on a
  micocontroller".
  
- A default protocol for interacting with applications running in the
  protocol, based on SLIP encoding to allow both UART and USB ACM.
  
- Relay board controller

- DHT11 / DHT22 temperature sensor interface

- A mini Forth interpreter

- A CSP scheduler (towards a CSP-based OS)

- Misc tools: Circular buffers, packet buffers, list, associative
  list, queue.

- Packet Bridge: a host tool to convert between different packet
  framing methods (SLIP, Erlang packet format, HEX, raw file
  descriptor, UDP, TAP).
  
- MDIO interface

- Preliminary Ethernet and UDP support

- Custom high-performance routines for STM32F103, optimized from
  generic OpenOCD code
  
- Preliminary no_std Rust code

- See also the erl_tools project which contains support code for
  integrating uC applications in a distributed Erlang network,
  
- See also the Staapl project, which contains some seed ideas for
  techniques used here, but using a custom Forth dialect instead of C.

