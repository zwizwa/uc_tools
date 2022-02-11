-- Compiler from block language to State Machine.  This has slightly
-- modified structure that cannot represent all of Scheme.  The two
-- main differences are:
--
-- 1. Non tail-recursive applications are inlined.
--
-- 2. Downward closures are allowed in functional loop combinators,
--    where they also will be aligned/specialized.

-- TODO:
-- . Non-recursive calls should just be substituted/inlined
-- . Add a parameterized continuation?  E.g. 1-deep CPS?

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
class.track_max = comp.track_max

local function ifte(c,t,f)
   if c then return t else return f end
end

local function _(expr)
   return l('_',expr)
end

local ephemeral = {
   ['closure'] = true,
   ['prim'] = true,
   ['void'] = true,
}

-- Compile a block with some arg manipulation at the start.
local function block_with_args(fun, list, tail)
   tail = tail or se.empty
   local i = 0
   return {'block', se.foldr(
      function(el, rest)
         local rv = fun(i, el)
         i = i + 1
         return {rv, rest}
      end,
      tail,
      list)}
end

-- Instantiate a function.
--
-- Note that we intentionally do NOT change environment to that of the
-- closure!  The C output only can support downward closures: every
-- variable that makes it into the code should be checked to make sure
-- it is defined in the lexical environment.  This is why we generate
-- the function instances right where the lambda used to be so its
-- variable references are legal.
--

function class.compile_fun(s, fun, label)
   return s:parameterize(
      {
         -- Expression is in tail position.
         tail = true,
         -- Don't touch the continuation.  It is hard-coded such that
         -- function return maps to a goto.
      },
      function()
         return
            l('label', label,
              s:comp(
                 block_with_args(
                    function(i, arg)
                       s:track_max("nb_args", i)
                       return l(arg, l('arg-ref', i))
                    end,
                    fun.args,
                    l(_(fun.body)))))
      end)
end



-- Goto with arguments.
function wrap_args(goto_or_label_expr, args)
   local expr =
      block_with_args(
         function(i, arg)
            return _(l('set-arg!', i, arg))
         end,
         args,
         l(_(goto_or_label_expr)))
   return expr
end

function class.make_var(s, src_name)
   return {
      class = 'var',
      unique = s:gensym(),
      var = src_name,
      iolist = frontend.var_iolist
   }
end

-- Propagate source names for debugging.
function class.set_debug_name(s, var, val)
   local typ = se.expr_type(val)
   if typ == 'closure' then
      val.debug_name = var.var
   end
end


function class.comp_bindings(s, bindings_list)
   return s:parameterize(
      {
         -- These variables are just saved and updated in-place.
         env  = s.env,
         cont = s.cont,
         tail = s.tail,
      },
      function()
         local up_var = s.cont
         local tail = s.tail
         local bindings = {}
         for binding, rest in se.elements(bindings_list) do
            local var, vexpr = se.unpack(binding, {n=2})
            s.tail = tail and se.is_empty(rest)
            if s.tail then
               assert(var == '_')
               s.cont = up_var
            elseif var ~= '_' then
               s.cont = var
            else
               s.cont = s:make_var("_")
            end

            -- FIXME: Handle forms here.  Primitive ones can be bound,
            -- others (if, app, block) will need custom continuations.
            -- FIXME: Actually, if there is a continuation here, we
            -- probably can drop the variable?  Or always emit it for
            -- later assignment?

            local vexpr1 = s:comp(vexpr)
            assert(vexpr1)
            local typ = se.expr_type(vexpr1)
            -- Record the value in the environment for later ref().
            if var ~= '_' then
               s:def(var, vexpr1)
            end
            if typ == 'closure' then
               local fun = vexpr1
               s:set_debug_name(s.cont, fun)
               -- Continuations are hardcoded.  We compile one
               -- instance per continuation, and use this map to
               -- indicate that a function has been compiled.
               -- Functions will be compiled later when they are
               -- referenced by the 'app' form.
               fun.compiled = {}
            elseif not ephemeral[typ] then
               -- Only collect concrete stuff.
               ins(bindings, l(var, vexpr1))
            end
         end
         return {'block',a2l(bindings)}
   end)
end


function class.compile_closure_app(s, fun, args)

   -- Function instances are specialized to the continuation,
   -- i.e. they 'return' through 'goto'.  The continuation can be
   -- mapped to the label if it was already compiled.
   local maybe_label = fun.compiled[s.cont]

   -- Use the label of the compiled function, or generate a new one in
   -- case we still need to compile it.
   local label = maybe_label or s:make_var(fun.debug_name)

   trace("LABEL",label)

   local function wrap_cont(expr)
      return expr
   end

   -- Together with a return point if the app is not in tail position.
   if not s.tail then
      assert(s.cont)
      -- Re-use the continuation variable's source name as the jump
      -- label's source name.
      local cont_label = s:make_var(s.cont.var)
      -- Attach the label to the cont var.  This instructs non-app
      -- tail position expressions to compile into a goto.
      s.cont.fun =
         function(val)
            return wrap_args(l('goto', cont_label), l(val))
         end

      -- And generate the label that will be jumped to.
      wrap_cont = function(expr)
         return
            l('block',
              _(expr),
              _(l('label', cont_label,
                  ifte(s.cont.var == '_',
                       l('block'),
                       l('set!', s.cont, l('arg-ref',0))))))
      end
   end

   local callexpr
   if not maybe_label then
      -- Fall through into the function body.
      fun.compiled[s.cont] = label
      callexpr = s:compile_fun(fun, label)
   else
      -- Jump to previously compiled body.
      callexpr = l('goto',label)
   end

   trace("APPBLOCK", outexpr)
   return wrap_cont(wrap_args(callexpr, args))
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
             return s:comp_bindings(m.bindings)
         end},
         {"(if ,c ,t ,f)", function(m)
             return l('if', m.c, s:comp(m.t), s:comp(m.f))
         end},
         {"(lambda ,args ,body)", function(m)
             -- Definitions are ephemeral.  Bodies will be compiled
             -- when they are referenced by 'app'.
             return { class = 'closure',
                      args = m.args,
                      body = m.body,
                      env = s.env }
         end},
         {"(set! ,var ,val)", function(m)
             local val = s:ref(m.val)
             s:set(m.var, val)
             s:set_debug_name(m.var, val)
             if m.var.var == '_' then
                -- This is used e.g. to represent a continuation that
                -- ignores the return value.
                return void
             else
                local typ = se.expr_type(val)
                if not ephemeral[typ] then
                   return expr
                else
                   return void
                end
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
                   return s:compile_closure_app(fun,m.args)
                else
                   error("bad func class '" .. fun.class .. "'")
                end
             end
         end},
         {",other", function(m)
             trace("OTHER",m.other)
             local typ = se.expr_type(m.other)
             if typ == 'var' and s.cont.fun then
                return s.cont.fun(m.other)
             else
                -- This is an ordinary block binding.
                return expr
             end
         end},
   })
end



function class.compile(s,expr)
   -- Prefix needs to be different from what is used in the frontend,
   -- so we don't clash.
   s.symbol_prefix = "l"

   -- Initialize gensym counter
   s.nb_sym = 0

   -- Top environment is empty.
   s.env = se.empty

   -- Goto return can be special
   s.ret = s:make_var('return')
   s.ret.fun = function(val) return l('return',val) end

   -- The 'var' parameter contains the continuation, which is a
   -- variable that will receive the value of the computation.  If
   -- s.cont.label is defined it contains the jump label, otherwise it
   -- is an ordinary block binding.
   s.cont = s.ret

   -- Tracks the maximum of arg-ref
   s.nb_args = -1

   -- Top expression is in tail position.
   s.tail = true

   return s.match(
      expr,
      {{"(lambda (,lib_ref) ,body)", function(m)
           -- The outer form is a lambda that binds the library lookup
           -- function, which gets passed the names of all the free
           -- variables in the original scheme code.
           local function lib_ref(name)
              return {
                 class = 'prim',
                 name = name,
                 iolist = {"prim:",name.expr}
              }
           end
           s:def(m.lib_ref, lib_ref)

           local c_body = s:comp(m.body)

           return l('block',
                    _(l('alloc_args', 1 + s.nb_args)),
                    _(c_body))
      end}})
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
