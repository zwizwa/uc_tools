# If the plugin link script corresponding to $BN does not exist, we
# come here for the default plugin link step, assuming the most common
# case: a RAM plugin linked to the current state of a microcontroller.

# FIXME: How to remove hard-coded state here?
STATE_DIR=~/lib/bluepill

PARENT_ELF=$STATE_DIR/$BN.app.elf
LINK_SH=./ram_plugin.link.sh
redo-ifchange $LINK_SH
. $LINK_SH
