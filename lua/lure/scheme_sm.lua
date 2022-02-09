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
local void = {class = 'void', iolist = "#<void>"}

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
end

class.def       = comp.def
class.ref       = comp.ref
class.set       = comp.set
class.find_cell = comp.find_cell
class.gensym    = comp.gensym
class.inc       = comp.inc

local function ifte(c,t,f)
   if c then return t else return f end
end

function frame(args, env)
   return {args, env}
end


local ephemeral = {
   ['closure'] = true,
   ['prim'] = true,
   ['void'] = true,
}


function class.compile_fun(s, fun, label)
   assert(nil == s.compiled[fun])
   s.compiled[fun] = {label = label}
   local i = 0
   local arg_bindings =
      se.map(
         function(arg)
            local b = l(arg, l('arg-ref', i))
            i = i + 1
            return b
         end,
         fun.args)
   ins(s.compiled_seq,
       l('_',
         l('label', label,
           s:comp({'block',
                   se.append(arg_bindings,
                             l(l('_', fun.body)))}))))
end

function class.comp(s, expr)

   local function lit_or_ref(thing)
      local typ = se.expr_type(thing)
      if typ == 'var' then return s:ref(thing)
      else return thing end
   end


   trace("COMP",expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             return s:parameterize(
                {
                   -- Save the environment here so it will be restored
                   -- on block exit.  We'll update the env one
                   -- variable at a time as we iterate through the
                   -- bindings.
                   env = s.env,

                },
                function()
                   local bindings = {}
                   for binding in se.elements(m.bindings) do
                      local var, vexpr = se.unpack(binding, {n=2})
                      local vexpr1 = s:comp(vexpr)
                      assert(vexpr1)
                      -- Define it for remaining expressions.
                      if var ~= '_' then
                         s:def(var, vexpr1)
                      end
                      local typ = se.expr_type(vexpr1)
                      trace("BINDING",l(var, vexpr1, typ))
                      log_desc(typ)
                      if not ephemeral[typ] then
                         -- Only collect concrete stuff.
                         ins(bindings, l(var, vexpr1))
                      end
                   end
                   return {'block',a2l(bindings)}
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
             local val = s:ref(m.val)
             s:set(m.var, val)
             if not ephemeral[se.expr_type(val)] then
                return expr
             else
                return void
             end
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
                local vals = se.map(lit_or_ref, m.args)
                return fun(unpack(l2a(vals)))
             else
                assert(fun.class)
                if fun.class == 'closure' then

                   -- Compile the function call
                   local i=0
                   for arg in se.elements(m.args) do
                      ins(seq, l('_',l('set-arg!',i,arg)))
                      i=i+1
                   end
                   local compiled = s.compiled[fun]
                   local label
                   if compiled then
                      label = compiled.label
                      assert(type(label) == "string")
                   else
                      label = s:gensym()
                   end
                   trace("LABEL",label)
                   ins(seq, l('_',l('goto',label)))

                   if not compiled then
                      -- Compile the function label + body.
                      if not s.compiled_seq then
                         -- Create an inlining context.
                         s:parameterize(
                            { compiled_seq = seq },
                            function()
                               s:compile_fun(fun, label)
                         end)
                      else
                         -- Already in an inlining context.  New labels
                         -- will be lifted.
                         s:compile_fun(fun, label)
                      end
                   end
                   local block ={'block',a2l(seq)}
                   trace("BLOCK", block)
                   return block
                else
                   assert(fun.class == 'prim')
                   return {fun, m.args}
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
   s.nb_sym = 0
   s.symbol_prefix = "l" -- only for labels

   s.compiled = {}

   return s.match(
      expr,
      {{"(lambda (,lib_ref) ,body)", function(m)
           -- The top form defines the library lookup function, which
           -- gets passed the names of all the free variables in the
           -- original scheme code.
           s:def(m.lib_ref, function(name)
                    return {
                       class = 'prim',
                       name = name,
                       iolist = {"prim:",name.expr}
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
