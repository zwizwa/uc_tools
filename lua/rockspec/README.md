To keep things simple, all dependencies are copied into a separate git
archive, which is then pushed to github.  The .rockspec refers to a
tag/version in the github repository. The conversion to .zip is then
done by the github server.

See e.g. ~/git/elfutils-lua/update.sh which copies files from
uc_tools, commits and pushes to github.

To install:

./install.sh elfutils-0.1-1.rockspec 
./install.sh lure-0.1-1.rockspec

