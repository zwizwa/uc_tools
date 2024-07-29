# If the plugin link script corresponding to $BN does not exist, we
# come here for the default plugin link step, assuming the most common
# case: a RAM plugin linked to the current state of a microcontroller.


# We assume LDT contains the uid of the uC, which we can use to look
# up the current application elf in the state cache.
# FIXME: remove hard-coded path
STATE_DIR=~/lib/bluepill

PARENT_ELF=$STATE_DIR/$LDT.app.elf

[ ! -f "$PARENT_ELF" ] && echo "plugin_link.sh: need PARENT_ELF" >&2 && exit 1


LINK_SH=./ram_plugin.link.sh
# LINK_SH=./flash_plugin.link.sh
redo-ifchange $LINK_SH
. $LINK_SH
