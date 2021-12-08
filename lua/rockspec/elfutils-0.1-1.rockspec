-- FIXME: I do not understand how to limit install to a single lua file.

package = "elfutils"
version = "0.1-1"

source = {
  url = "https://github.com/zwizwa/uc_tools/archive/e966333a2708d368bf7de12c5847a36e4e1191f5.zip",
  dir = "uc_tools-e966333a2708d368bf7de12c5847a36e4e1191f5",
}

description = {
  summary    = "Lua code from uc_tools",
  homepage   = "https://github.com/zwizwa/uc_tools",
  license    = "MIT/X11",
  maintainer = "Tom Schouten",
  detailed   = [[
  Please note that this is very experimental software.
  ]],
}

dependencies = {
  "lua >= 5.1, < 5.5"
}

build = {
  type = "builtin",

  -- If this is not included it includes everything?
  copy_directories = {"lua/elfutils"},

  platforms = {
    linux   = { modules = {
      elfutils_lua51 = {
        libraries = {"elf", "dw"},
      }
    }},
  },

  modules = {
    elfutils_lua51 = {
      sources = {
        "linux/elfutils_lua51.c",
      },
      incdirs   = { ".", ".." },
      libdirs   = { }
    },
    elfutils = "lua/lib/elfutils.lua",
  }

}
