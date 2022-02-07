-- FIXME: Basic idea looks ok. Details are wrong.

-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives implementing 'base-ref' free variables.
-- . Relies on unique renamed variables: interpreter does not track environment
-- . Simpler to implement this in Lua, using pattern matching library.

local se       = require('lure.se')
local se_match = require('lure.se_match')

local l2a = se.list_to_array

local class = {}

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
end

function class.eval_loop(s, expr)

   local retvar = { }
   local k = { var = retvar }

   -- Continuations are chained.
   local function ret(val)
      k.var.val = val
      expr      = k.expr
      k         = k.nxt
   end

   local function base_ref(name)
      assert(type(name) == 'string')
      local fun = s.prim[name]
      if not fun then
         error("primitive '" .. name .. "' not defined")
      end
      trace("PRIM",name)
      return fun
   end

   local function ref(var)
      assert(var.class == 'var')
      if var.var == 'base-ref' then return base_ref end
      local val = var.val ; assert(val)
      return val
   end

   local function lit_or_ref(thing)
      if type(thing) ~= 'table' then return thing end
      local class = thing.class
      assert(class)
      if 'var' == class then
         return ref(thing)
      elseif 'expr' == class then
         return thing.expr
      else
         error("lit_or_ref, bad class '" .. class .. "'")
      end
   end

   repeat
      trace("EVAL", expr)
      s.match(
         expr,
         {
            {"(block)", function(m)
                ret(void)
            end},
            {"(block (_ ,expr))", function(m)
                expr = m.expr
            end},
            {"(block (,var ,expr))", function(m)
                error("last expression in 'block' is bound: '" .. m.var .. "'")
            end},
            {"(block (_ ,expr) . ,rest)", function(m)
                expr = m.expr
                k = { expr = {'block',m.rest}, var = {}, nxt = k }
            end},
            {"(block (,var ,expr) . ,rest)", function(m)
                expr = m.expr
                k = { expr = {'block',m.rest}, var = m.var, nxt = k }
            end},
            {"(if ,cond ,iftrue ,iffalse)", function(m)
                expr = ifte(lit_or_ref(m.cond), m.iftrue, m.iffalse)
            end},
            {"(set! ,var ,val)", function(m)
                m.var.val = m.val
                ret(void)
            end},
            {"(lambda ,args ,body)", function(m)
                ret(m)
            end},
            {"(,fun . ,args)", function(m)
                local fun = lit_or_ref(m.fun)
                local vals = se.map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   ret(fun(unpack(l2a(vals))))
                else
                   se.zip(
                      function(var, val)
                         var.val = val
                         -- FIXME list vs.array
                      end, fun.args, vals)
                   expr = fun.body
                end
            end},
            {",atom", function(m)
                ret(lit_or_ref(m.atom))
            end},
         })
      until (not k)
      return retvar.val
end


function class.eval(s,expr)
   return s:eval_loop(expr)
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
