How to release
--------------

The example here uses version 0.2 which should be replaced with the
new version.

Note that luarocks server allows to update versions in place, but it
is probably a good idea to always bump version.  At the very least,
bump versions if API changed.

1. Create a new file in the uc_tools repository
   `uc_tools/lua/rockspec/elfutils-0.2-1.rockspec`
   
   This can be loned from the previous file with all version strings replaced.
   This includes `version`, `source.url` and `source.dir`
   
3. Create a new branch `v0.2` in the stripped down repository `elfutils-lua`

    $ git checkout -b v0.2
    
4. Then run the update script in that repository

    $ ./update.sh
    

6. Upload the `.rockspec` file manually to `luarocks.org`

7. Test it in `lua/rockspec` directory

    $ ./install.sh elfutils-0.2-1.rockspec
    
8. Add new version to nix builds



    


