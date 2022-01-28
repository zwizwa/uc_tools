-- Macro stepper to test lib.scheme_macros

local iolist   = require('lib.iolist')
local macros   = require('lib.scheme_macros')
local se       = require('lib.se')
local l = se.list
require('lib.log')
local function log_w(...)   iolist.write(log, {...}) end
local function log_se(expr) log_w(se.iolist(expr)) end

local config = {
   case = { gensym = function() return "r0" end }
}

local function macro_step(expr)
   assert(expr and not se.is_empty(expr))
   local form = se.car(expr)
   assert(form and type(form) == 'string')
   local macro = macros[form]
   assert(macro)
   return macro(expr, config[form])
end

local prim = {
   ['let*']   = true,
   ['lambda'] = true,
   ['if']     = true,
   ['set!']   = true,
}

local function expand(stepped)
   local i = 1
   while true do
      if nb and i > nb then
         log_w("stop at i = ", i, "\n")
         return
      end
      if type(stepped) ~= 'table' then
         log_w("no expr\n");
         return
      end
      local form = se.car(stepped)
      if prim[form] then
         -- log_w("prim = ", form, "\n")
         return
      end
      stepped = macro_step(stepped)
      log(" -> ") ; log_se(stepped) ; log("\n")
      i = i + 1
   end

end

local function t(str, nb)
   if not nb then nb = 10 end
   local stream = se.string_to_stream(str)
   local parse = se.new(stream)
   local expr = parse:read(str)
   log_se(expr) ; log("\n")
   return expand(expr)
end

local function test()
   t("(module-begin 1 2)")
   t("(letrec ((a 1) (b 2)) a)")
   t("(begin)")
   t("(begin 123)")
   t("(begin 1 2 3)")
   t("(letrec ((a 1) (b 2)))")
   t("(letrec ())")
   t("(begin (define (a x) (b x)) (define (b x) (a x)) a)")
   t("(case 1 ((0) a) ((1) b))")
end

test()

