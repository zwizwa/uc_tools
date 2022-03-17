-- C Compiler Interface.

-- Currently the main purpose of this code is to test generated C
-- code.  To avoid binary interfaces, it seems simplest compile to a
-- stand-alone binary, and have that read s-expressions and produce
-- s-expressions.

-- Alternatively, a finer grained execution pattern is possible by
-- running it in GDB, e.g. with scheme_gdb generated commands.

local iolist = require('lure.iolist')
local se     = require('lure.se')
local asset  = require('lure.asset_c')
local mod = {}

require('lure.log_se')

local l = se.list

local function shell(cmd_iol)
   local cmd = iolist.to_string(cmd_iol)
   local f = io.popen(cmd)
   local out = f:read("*a")
   assert(out)
   f:close()
   -- Can't capture error code in Lua 5.1, so map an empty output
   -- string to false.
   return out ~= "" and out
end

function mod.version()
   return shell({"gcc --version | head -n1"})
end

function mod.eval_se(c_code_iol, in_se, cflags_iol)
   local out = mod.eval_iol(c_code_iol, se.iolist(in_se), cflags_iol)
   return out and se.read_string_multi(out)
end

function mod.eval_iol(c_code_iol, in_iol, cflags_iol)
   assert(mod.version())
   local c   = "main.c"
   local elf = "main.elf"
   cflags_iol = cflags_iol or ""
   return shell({
         "set -e\n",     -- exit on first error
         -- "set -x\n",     -- verbose
         -- Create tempdir.
         "DIR=$(mktemp -d /tmp/lure_cc.XXXXX)\n",
         -- "echo -n $DIR"
         "C=$DIR/",c,"\n",
         "ELF=$DIR/",elf,"\n",
         "cat <<EOF >$C\n",
         asset['scheme_c.h'],
         c_code_iol,
         "\nEOF\n",
         -- "cat $C >&2\n",
         "gcc ", cflags_iol, "-Os -o $ELF $C\n",
         -- "objdump -d $ELF\n",
         "cat <<EOF | $ELF\n",
         in_iol,
         "EOF\n",
         "rm -rf $DIR\n",
   })
end

function mod.test()
   log_desc({version = mod.version()})
   local out = mod.eval_iol(
      {
         '#include <stdio.h>\n',
         'int main(int argc, char **argv) { printf("(1 2 3)"); return 0; }\n'
      },
      "1 2 3"
   )
   log(out)
end

return mod

