local iolist   = require('lib.iolist')
local macros   = require('lib.scheme_macros')
local se       = require('lib.se')
local l = se.list
require('lib.log')
local function log_w(...)   iolist.write(log, {...}) end
local function log_se(expr) log_w(se.iolist(expr)) end

local function macro_step(expr)
   assert(expr and not se.is_empty(expr))
   local form = se.car(expr)
   assert(form and type(form) == 'string')
   local macro = macros[form]
   assert(macro)
   return macro(expr)
end

local function t(str, nb)
   local stream = se.string_to_stream(str)
   local parse = se.new(stream)
   local expr = parse:read(str)
   log_se(expr)
   log("\n")
   local stepped = expr
   for i=1,nb do
      log(" -> ")
      stepped = macro_step(stepped)
      log_se(stepped)
      log("\n")
   end
end

local function test()
   t("(letrec ((a 1) (b 2)) a)",1)
   t("(begin)",1)
   t("(begin 123)",2)
   t("(begin 1 2 3)",2)
end

test()

