# -*- gdb -*-
set confirm off
set pagination off

# tools
define file_check
  echo file=
  echo $arg0
  echo \n
  file $arg0
end

# shorthand
define comp
  compare-sections
end
define lc
  load
  comp
end

# move cursor to home position and clear screen
define clear-screen
  echo \033[H\033[2J
end

define xi
  x/10i $pc-10
end

define s
  clear-screen
  stepi
  xi
end

# set breakpoint at reset
#
# the reset vector is stored at 0x08000004, hence the double asterisk:
# the first one is to get the address, the second one is because gdb
# breakpoints want a dereference (seems you set a breakpoint at an
# instruction, not at the address of an instruction)
#
# this is useful for printing logs after a crash.
#
# "p semihosting_info_poll()" still works because sram is intact
#
# note that you want to pipe semihosting output (openocd stdout)
# through "uc_trace/format.sh -" to get human-readable messages.
#
define break-at-reset
  break **0x08000004
end


# Null pointer dereferences end up at:
# (gdb) break blocking_handler
