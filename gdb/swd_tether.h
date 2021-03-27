/* SWD Tether

   Currently just an idea, but the general context is:

   - uc_tools is built around the availability of a packet transport
     to/from a host machine.  This is typically USB

   - USB isn't always available, but SWD usually is

   Doesn't take long to connect the dots and allow for the
   transportation of USB style packets over SWD.  Especially when the
   design of the Black Magic Probe is considered: two TTY ACM
   endpoints, one with GDB RSP, and one for the target's serial port.
   Repurposing that port for the uc_tools protocols seems quite
   straightforward.

   So how to make it work?  With some target library support and the
   use of software interrupts, the following should be sufficient:

   - USB OUT ISR on BMP clocks moves the packet into the target's
     memory, and toggles the software interrupt when it's done.

   - USB IN ISR on BMP polls the target for packet ready condition,
     and transfers it from target's memory when ready, toggling a
     software interrupt when done.

   This mainly needs a synchronization mechanism between the SWD
   access from the main GDB loop, and the interrupt handlers for the
   second endpoint.

*/
