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
   --['closure'] = true,
   --['prim'] = true,
   --['void'] = true,
}

-- Move to se.lua
local function map0(fun, list)
   local i = 0
   return se.map(
      function(el)
         local rv = fun(i, el)
         i = i + 1
         return rv
      end,
      list)
end

function class.compile_fun(s, fun, label)
   s:parameterize(
      {
         tail = true,
         var  = nil, -- FIXME
      },
      function()
         assert(nil == s.context.fun[fun])
         s.context.fun[fun] = {label = label}
         local arg_bindings = map0(
            function(i, arg)
               return l(arg, l('arg-ref', i))
            end,
            fun.args)
         ins(s.context.seq,
             l('_',
               l('label', label,
                 s:comp({'block',
                         se.append(arg_bindings,
                                   l(l('_', fun.body)))}))))
      end)
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
                   -- on block exit.  It's more convenient to update
                   -- the env one variable at a time as we iterate
                   -- through the bindings, as opposed to inserting
                   -- s:parameterize for each binding.
                   env  = s.env,
                   var  = s.var,
                   tail = s.tail,
                },
                function()
                   local up_var = s.var
                   local tail = s.tail
                   local bindings = {}
                   for binding, rest in se.elements(m.bindings) do
                      local var, vexpr = se.unpack(binding, {n=2})
                      s.tail = tail and se.is_empty(rest)
                      if s.tail then
                         assert(var == '_')
                         s.var = up_var
                      else
                         s.var = var
                      end
                      local vexpr1 = s:comp(vexpr)
                      assert(vexpr1)
                      -- Define it for remaining expressions.
                      if var ~= '_' then
                         s:def(var, vexpr1)
                      end
                      local typ = se.expr_type(vexpr1)
                      trace("BINDING",l(var, vexpr1, typ))
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
             local i=0
             local fun = s:ref(m.fun)
             trace("APP",l(m.fun, fun))
             if type(fun) == 'function' then
                -- Primitive functions can be ephemeral and/or emit code.
                local vals = se.map(lit_or_ref, m.args)
                return fun(unpack(l2a(vals)))
             else

                assert(fun.class)

                if fun.class == 'prim' then
                   return {fun, m.args}

                elseif fun.class == 'closure' then


                   -- Compile function entry: set argument registers +
                   -- goto.  If the function has not yet been
                   -- compiled, then emit a (label) expression.


                   -- Behavior of closures depends on how they are
                   -- called.  For calls in tail position, a goto is
                   -- inserted and the body of the code is compiled
                   -- into the local function block.  For non-tail
                   -- calls, a new empty function block is started
                   -- together with an exit continuation. Note the
                   -- similarity between creating stack frames and
                   -- creating new compilation contexts.



                   function ins_call()
                      local seq = {}

                      -- Compile the function call: set arguments.
                      map0(
                         function(i, arg)
                            ins(seq, l('_', l('set-arg!', i, arg)))
                         end,
                         m.args)
                      -- Jump to label.  Create label if the function is
                      -- not yet compiled.
                      local compiled = s.context.fun[fun]
                      local label = (compiled and compiled.label) or s:gensym()
                      trace("LABEL",label)
                      ins(seq, l('_',l('goto',label)))

                      if not compiled then
                         s:compile_fun(fun, label)
                      end

                      local block ={'block',a2l(seq)}
                      trace("BLOCK", block)
                      return block

                   end

                   if s.tail then
                      return ins_call()
                   else
                      return s:comp_new_context(ins_call)
                   end

                else
                   error("bad func class '" .. fun.class .. "'")
                end
             end
         end},
         {",other", function(m)
             return expr
         end},
   })
end

function class.comp_new_context(s, fun)

   -- The continuation label for the end of the loop.
   -- local k_label = s:gensym()
   local seq = {}
   local k_var = s.var
   -- Keep them associated.
   local k_label = (k_var and k_var.unique) or s:gensym()

   return s:parameterize(
      { context = { fun = {}, seq = seq, k_label = k_label } },
      function()

         local expr1 = fun()
         ins(seq, l('_', expr1))

         -- Compile the continuation.
         -- FIXME: This could do multiple arguments.
         -- FIXME: It doesn't need to use the arg, can set var directly.
         ins(seq, l('_',l('label', k_label,
                          l('block',
                            l('_',l('set!',k_var,l('ref-arg',0)))))))

         return {'block',a2l(seq)}
      end)
end



-- FIXME: Start with top level function body that is compiled into a
-- s.context.seq then on each non-tail call, recurse starting a new
-- s.context.seq.

function class.compile(s,expr)
   s.env = se.empty
   s.nb_sym = 0
   s.symbol_prefix = "l" -- only for labels

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
           return s:comp_new_context(
              function()
                 return s:comp(m.body)
              end)
      end}})
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
