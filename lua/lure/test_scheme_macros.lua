-- Macro stepper to test lure.scheme_macros

local iolist   = require('lure.iolist')
local macros   = require('lure.scheme_macros')
local se       = require('lure.se')
local l = se.list

require('lure.log')
local function log_w(...)   iolist.write(log, {...}) end
local function log_se(expr) log_w(se.iolist(expr)) end

function gensym(s)
   s.n = s.n + 1
   return "r" .. s.n
end
local state = { n = 0, gensym = gensym }
local function cfg(c)
   c.state = state
   return c
end

local config = {
   ['let']  = cfg({ named_let_trampoline = 'named-let-trampoline' }),
}

local function macro_step(expr)
   assert(expr and not se.is_empty(expr))
   local form = se.car(expr)
   assert(form and type(form) == 'string')
   local macro = macros[form]
   assert(macro)
   local cfg = config[form] or cfg({})
   assert(cfg)
   return macro(expr, cfg)
end

local prim = {
   ['block']  = true,
   ['lambda'] = true,
   ['if']     = true,
   ['set!']   = true,
   -- For implementing trampoline
   ['named-let-trampoline'] = true,
   ['quote']  = true,
   ['unquote']  = true,
   ['quasiquote']  = true,
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
      if type(form) ~= 'string' then
         log_w("no form\n");
         return
      end
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
   local expr = se.read_string(str)
   log_se(expr) ; log("\n")
   return expand(expr)
end

local function test()
   -- reader test
   t("'(a b c)")
   t(",(a b c)")
   t("`(a b c)")
   t("'(a . (b c))")
   t("`(a . ,b)")

   t("(module-begin 1 2)")
   t("(letrec ((a 1) (b 2)) a)")
   t("(begin)")
   t("(begin 123)")
   t("(begin 1 2 3)")
   t("(letrec ((a 1) (b 2)))")
   t("(letrec ())")
   t("(begin (define (a x) (b x)) (define (b x) (a x)) a)")
   t("(case 1 ((0) a) ((1) b))")
   t("(let loop ((n 0) (a 2)) (if (n > 3) a (loop (+ n 1) (* a a))))")
   t("(begin (define (decode-loop stack) (define (sym n) 123) 456))")
   -- t("(let ((x 123)) (define (y a) a) x)")
   t("(let ((a a1) (b b1)) 123)")
   t("(case x ((0) a b) ((1) d e))")
   t("(let* ((a 1) (b 2)) a b)")
   t("(or a b)")
   t("(and a b)")

end

return {
   run = test
}

