-- C Compiler Interface.
local iolist = require('lure.iolist')
local se     = require('lure.se')
local mod = {}

require('lure.log_se')

local l = se.list

-- For testing, the most convenient is to map C code + s expression
-- input to s expression output.  To avoid messing with dynamic
-- linking, it seems simplest to generate a stand-alone elf, dump the
-- s-expression to its input (or some translated version), and record
-- its output.  Alternatively, run it in gdb?
local function shell(cmd_iol)
   local cmd = iolist.to_string(cmd_iol)
   local f = io.popen(cmd)
   local out = f:read("*a")
   assert(out)
   f:close()
   return out
end

function mod.eval(c_code_iol, in_iol, cflags_iol)
   local c   = "main.c"
   local elf = "main.elf"
   cflags_iol = cflags_iol or ""
   return shell({
         "set -e\n",     -- exit on first error
         "set -x\n",     -- verbose
         -- Create tempdir.
         "DIR=$(mktemp -d /tmp/lure_cc.XXXXX)\n",
         -- "echo -n $DIR"
         "C=$DIR/",c,"\n",
         "ELF=$DIR/",elf,"\n",
         "cat <<EOF >$C\n",
         c_code_iol,
         "\nEOF\n",
         "cat $C >&2\n",
         "gcc ", cflags_iol, "-Os -o $ELF $C\n",
         -- "objdump -d $ELF\n",
         "cat <<EOF | $ELF\n",
         in_iol,
         "EOF\n",
         "rm -rf $DIR\n",
   })
end

function mod.test()
   local out = mod.eval(
      {
         '#include <stdio.h>\n',
         'int main(int argc, char **argv) { printf("(1 2 3)"); return 0; }\n'
      },
      "1 2 3"
   )
   log(out)
end

return mod

