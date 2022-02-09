-- Compiler from block language to State Machine.  This has slightly
-- modified structure that cannot represent all of Scheme.  The two
-- main differences are:
--
-- 1. Non tail-recursive applications are inlined.
--
-- 2. Downward closures are allowed in functional loop combinators,
--    where they also will be aligned/specialized.



local se       = require('lure.se')
local comp     = require('lure.comp')
local se_match = require('lure.se_match')

local ins = table.insert
local l2a = se.list_to_array
local a2l = se.array_to_list
local l = se.list

local class = {}

class.parameterize = comp.parameterize

local void = "#<void>"

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
end

class.def = comp.def
class.ref = comp.ref
class.set = comp.set
class.find_cell = comp.find_cell

local function ifte(c,t,f)
   if c then return t else return f end
end

function frame(args, env)
   return {args, env}
end


-- function class.comp_extend(s, expr, vars)

--    local new_env = s.env
--    for var in se.elements(vars) do
--       new_env = {var, new_env}
--    end
--    return s:parameterize(
--       {env = new_env},
--       function()
--          -- log_se_n(expr, "COMPILE_EXTEND:")
--          return s:comp(expr)
--       end)


function class.comp(s, expr)
   trace("COMP",expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             return s:parameterize(
                -- Save the environment here so it will be restored on
                -- block exit.  We'll update the env one variable at a
                -- time as we iterate through the bindings.
                {env = s.env},
                function()
                   local bindings =
                      se.map(
                         function(binding)
                            local var, vexpr = se.unpack(binding, {n=2})
                            local vexpr1 = s:comp(vexpr)
                            assert(vexpr1)
                            -- Define it for remaining expressions.
                            if var ~= '_' then
                               s:def(var, vexpr1)
                            end
                            return l(var, vexpr1)
                         end,
                         m.bindings)
                   -- trace("BLOCK",bindings)
                   return {'block',bindings}
                end)
         end},
         {"(if ,c ,t ,f)", function(m)
             return l('if', m.c, s:comp(m.t), s:comp(m.f))
         end},
         {"(lambda ,args ,body)", function(m)
             -- At this point all functions will need to be first
             -- order.  Definitions are ephemeral.  Bodies will be
             -- compiled once they are applied.
             return { class = 'closure',
                      args = m.args,
                      body = m.body,
                      env = s.env }
         end},
         {"(set! ,var ,val)", function(m)
             s:set(m.var, s:ref(m.val))
             -- FIXME: This needs to distinguish ephemeral variables
             -- from variables that make it through to the code.
             return expr
         end},
         {"(app ,fun . ,args)", function(m)
             -- Compile function entry: set argument registers + goto.
             -- If the function has not yet been compiled, then emit a
             -- (label) expression.
             local seq = {}
             local i=0
             local fun = s:ref(m.fun)
             trace("APP",l(m.fun, fun))
             if type(fun) == 'function' then
                return fun
             else
                assert(fun.class)
                if fun.class == 'closure' then
                   -- local fun_val = ref(m.fun)
                   for arg in se.elements(m.args) do
                      ins(seq, l('_',l('set-arg!',i,arg)))
                      i=i+1
                   end
                   ins(seq, l('_',l('goto',m.fun.unique)))
                   return {'block',a2l(seq)}
                else
                   return fun
                end
             end
         end},
         {",other", function(m)
             return expr
         end},
   })
end

function class.compile(s,expr)
   s.env = se.empty
   return s.match(
      expr,
      {{"(lambda (,lib_ref) ,body)", function(m)
           -- The top form defines the library lookup function, which
           -- gets passed the names of all the free variables in the
           -- original scheme code.
           s:def(m.lib_ref, function(name)
                    return {
                       class = 'lib',
                       iolist = {"#<",name,">"}
                    }
           end)
           return s:comp(m.body)
      end}})
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
