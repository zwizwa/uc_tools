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
local frontend = require('lure.scheme_frontend')

local ins = table.insert
local l2a = se.list_to_array
local a2l = se.array_to_list
local l = se.list

local class = {}

class.parameterize = comp.parameterize
local void = {class = 'void', iolist = "#<void>"}

local function trace(tag, expr)
   -- log_se_n(expr, tag .. ":")
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

function class.make_var(s, src_name)
   return {
      class = 'var',
      unique = s:gensym(),
      var = src_name,
      iolist = frontend.var_iolist
   }
end


-- Note that we do NOT change environment to that of the closure.  The
-- C output only can support downward closures: every variable that
-- makes it into the code should be checked to make sure it is defined
-- in the lexical environment.



function class.compile_fun(s, fun, label)
   s:parameterize(
      {
         tail = true,
      },
      function()
         local arg_bindings = map0(
            function(i, arg)
               return l(arg, l('arg-ref', i))
            end,
            fun.args)
         local compiled =
            l('label', label,
              s:comp({'block',
                      se.append(arg_bindings,
                                l(l('_', fun.body)))}))

         -- {'block' . tail}
         fun.add_instance(compiled)

         --ins(s.context.seq,
         --    l('_', compileD))
      end)
end


-- Propagate source names for debugging.
function class.set_debug_name(s, var, val)
   local typ = se.expr_type(val)
   if typ == 'closure' then
      val.debug_name = var.var
   end
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
                      elseif var ~= '_' then
                         s.var = var
                      else
                         s.var = s:make_var("_")
                      end

                      local vexpr1 = s:comp(vexpr)
                      assert(vexpr1)
                      -- Define it for remaining expressions.
                      if var ~= '_' then
                         s:def(var, vexpr1)
                      end
                      local typ = se.expr_type(vexpr1)
                      if typ == 'closure' then
                         local fun = vexpr1
                         s:set_debug_name(s.var, fun)
                         -- Closures will only be accessed through
                         -- ref().  The code representation is a block
                         -- that contains one or more instantiations.
                         -- Link the closure object to this reservoir
                         -- for later compilation.  It is done this
                         -- way to ensure that compiled functions will
                         -- be in the correct scope.  Currently not
                         -- clear if more than one instance is
                         -- necessary.

                         -- FIXME: There should be at least one
                         -- instance per continuation, as the jump is
                         -- hardcoded.  Maybe continuation can
                         -- actually be dynamic now?

                         local instances = l('block')
                         local function add_instance(expr)
                            se.push_cdr(l('_',expr), instances)
                         end
                         fun.add_instance = add_instance
                         fun.compiled = false
                         -- FIXME: Jump over the block.
                         ins(bindings, l('_', l('if',0,0,instances)))
                      elseif not ephemeral[typ] then
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
             local typ = se.expr_type(val)
             s:set_debug_name(m.var, val)
             if not ephemeral[typ] then
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



                   local seq = {}

                   -- Compile the function call: set arguments.
                   map0(
                      function(i, arg)
                         ins(seq, l('_', l('set-arg!', i, arg)))
                      end,
                      m.args)

                   -- FIXME: Currently not really clear how to see if
                   -- more instances are needed, so just sticking to
                   -- one instance.


                   -- Jump to label.  Create label if the function is
                   -- not yet compiled.
                   local compiled = fun.compiled
                   local label = compiled or s:make_var(fun.debug_name)

                   trace("LABEL",label)
                   ins(seq, l('_',l('goto',label)))

                   if not s.tail then
                      -- If not a tail call, compile and set the continuation.
                      assert(s.var)
                      -- Reuse tha name for the label
                      local cont_label = s:make_var(s.var.var)
                      -- Attach the label to the cont var.
                      s.var.label = cont_label
                      -- And generate the code
                      ins(seq, l('_',l('label', s.var.label, l('set!', s.var, l('ref-arg', 0)))))
                   end

                   if not compiled then
                      -- This will stop recursive calls from compiling again
                      fun.compiled = label
                      s:compile_fun(fun, label)
                   end

                   local block = {'block',a2l(seq)}
                   trace("APPBLOCK", block)
                   return block
                else
                   error("bad func class '" .. fun.class .. "'")
                end
             end
         end},
         {",other", function(m)
             local typ = se.expr_type(m.other)
             if typ == 'var' and s.var.label then
                -- This is a return from the current mutrec context.
                if s.var == s.ret then
                   return l('return',m.other)
                else
                   return l('block',
                            l('_',l('set-arg!',0,m.other)),
                            l('_',l('goto',s.var.label)))
                end
             else
                return expr
             end
         end},
   })
end



-- FIXME: Start with top level function body that is compiled into a
-- s.context.seq then on each non-tail call, recurse starting a new
-- s.context.seq.

function class.compile(s,expr)
   -- Prefix needs to be different from what is used in the frontend,
   -- so we don't clash.
   s.symbol_prefix = "l"
   s.env = se.empty
   s.nb_sym = 0

   -- Goto return can be special
   s.ret = s:make_var('return')
   s.ret.label = s.ret.var
   s.var = s.ret
   s.tail = true

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
