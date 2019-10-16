# This file is included from default.elf.do whenever $LDT==custom,
# e.g.: redo plugin_example.custom.f103.elf

# The main reason to perform custom linking is to allow for "plugins"
# that can be loaded along side a main application, and that can call
# into the application code as well.  There are too many degrees of
# freedom to organize this in a generic way, so we resort fo a custom
# link step that is associated to the main .c file.


PARENT_ELF=doodle.x8.f103.elf
redo-ifchange $PARENT_ELF

# FIXME: These need to not overlap with parent elf.
RAM=0x20004000
FLASH=0x08008000

LD=$3.tmp.ld
cat <<EOF >$LD
MEMORY /* STM32F103x8 */
{
	rom (rx)  : ORIGIN = $FLASH, LENGTH = 0xFFFFFFFF
	ram (rwx) : ORIGIN = $RAM,   LENGTH = 0xFFFFFFFF
}
INCLUDE stm32f1.ld
EOF

$GCC $LDFLAGS -T$LD -Wl,-Map=$BN.$ARCH.map  -Wl,--just-symbols=$PARENT_ELF -o $3 $O $A $LDLIBS && rm $LD


# See below for more information.  This is an example of a "snippet"
# file and its associated build script.  I've not found a better
# approach than to autogenerate the associated .do file, keeping the
# implementation as "flat" as possible, because there are too many
# variables to find a good hierarchical implementation.

# Short story: if you have ex_snippet.c, generate a corresponding
# ex_snippet.ex_snippet_ldt.arch.elf.do and run:
# redo exo_snippet.ex_snippet_ldt.elf




# ARCH=f103

# ENV=./env.$ARCH.sh
# redo-ifchange $ENV
# . $ENV
# O=exo_snippet.$ARCH.o
# redo-ifchange $O

# LD=exo



# A "snippet", for lack of a better name, is a small piece of code
# that has a fixed location on the target and can easily be erased and
# reflashed.  It has a RAM and Flash start address, and optionally a
# size as well.

# Integrated, the workflow looks like this:
# - modify C file
# - if it is a snippet file, scan all bluepill apps in exo
# - for each running app that is associated to this file:
#   - get the snippet slot (e.g. top of RAM, Flash from .elf)
#   - create a linker script for that slot
#   - compile the C file for the architecture (e.g. f103)
#   - link it into an .elf using the generated linker script
#   - load it via gdb, or, convert to binary and push it via gdbrsp

# So, does this actually fit in the current naming scheme?
# <snippet>.<parent_elf>.<arch>.elf

# No, because parent_elf itself has a <name>.<ldt>.<arch>.elf name and
# thus doesn't fit into a single linker script type (ldt).

# An intermediate ldt needs to be created that can be expanded to:
# - parent elf (name, ldt, arch)
# - the "slot", if different from the main one just past the parent elf

# It seems simplest to put all that into an env.<snippet_name>.sh file
# that is included in default.elf.do

# Workflow:
# - create: env.<snippet_ldt>.sh containing all necessary variables
# - run: redo <snippet_name>.<snippet_ldt>.<arch>.elf

