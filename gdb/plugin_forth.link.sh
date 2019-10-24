# The only thing we need to do here is to map the BN (plugin_forth) to
# the ELF that will be loading the plugin.  The plugin code can use
# symbols defined in the elf.
PARENT_ELF=lab_board.x8.f103.elf

# Keep this configurable
# LINK_SH=flash_plugin.link.sh
LINK_SH=ram_plugin.link.sh

# The actual linking step and .ld script generation is common for all
# plugins, so just source it here.
redo-ifchange $LINK_SH
. ./$LINK_SH


