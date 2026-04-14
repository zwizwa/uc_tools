This is just for playing, rekindling the flame.

I turned my hobby into my work and I lost my hobby.

But, for some reason, messing with these old 32 bit machines brings me
back to when I was 16 years old and full of joy about discovering low
level programming.

After switching to NixOS, maintaining a lot of special-purpose
machines became fairly trivial, but I do find that maintaining
separate binary architectures does not fit in the flow very well, so
all my NixOS is 64 bit.  So in my mind, a PC is 64 bit and everything
else is an embedded system, or "an appliance".  Basically, a computer
that is not used for general purpose work

Further simplifying, "an appliance" is either a Linux system or a bare
metal or RTOS setup.

Recently I also figured out how to use Nix on ARM, MIPS to be able to
create self-contained bundles on a kernel that is provided by another
build setup (buildroot, OpenWRT, ...).  This was easily generalized to
i686 with Tiny Core Linux as base, which is what I've been using for a
recent floppy archiving project and general messing around with old
hardware.

But the itch remained to do something bare metal, since that is really
where my inner 16 year old got hooked on.  It made sense to just try
to get something running and that is what eventually got integrated
here, starting from a small boot loader that can do PXE and floppy
boot, switch to 32 bit protected mode and run some C code, bringing it
into uc_tools territory.

Some ideas:
- Get basic bare metal setup working, uc_tools style
- Boot via PXE, floppy and HD if needed, or find a way to make a .lkrn image
- Try to port a network driver for some real-time ethernet work
- Figure out interrupts in protected mode
- How to access VESA modes?  Can I program an intel GPU directly?
- Get the EMU10K to do something in response to network or serial commands
- Get to know QEMU better
- Play with Claude to figure out the obscure details
- Provide a base layer to run Rust Embassy
