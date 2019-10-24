# The only thing we need to do here is to map the BN (plugin_forth) to
# the ELF that will be loading the plugin.  The plugin code can use
# symbols defined in the elf.

# Link against active build
PARENT_ELF=lab_board.x8.f103.elf

# Link against a specific uC state.  FIXME: This needs to be organized
# better, but the idea is that it is (almost) correct to use the
# current Flash state of the uC.  However, it's probably best to then
# name this $UID.link.elf and create a symlink to the C file.
# PARENT_ELF=~/lib/bluepill/56ff6f065078525347130887.elf



# Keep this configurable
# LINK_SH=flash_plugin.link.sh
LINK_SH=ram_plugin.link.sh

# The actual Flash and RAM linking steps and .ld script generation are
# common for all plugins.
redo-ifchange $LINK_SH
. ./$LINK_SH



