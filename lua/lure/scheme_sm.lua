-- Compiler from block language to State Machine.  This has slightly
-- modified structure that cannot represent all of Scheme.  The two
-- main differences are:
--
-- 1. All function applications are inlined, with support for loops.
--
-- 2. Downward closures are allowed.
--


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

local function closure_iolist(c)
   if c.debug_name then
      return {"#<closure:",c.debug_name,">"}
   else
      return {"#<closure>"}
   end
end

local ephemeral = {
   ['closure'] = true,
   ['prim'] = true,
   ['void'] = true,
}

-- Compile a block with some statements at the beginning.  This is
-- used for arg-ref and set-arg!.
local function block_enter(fun, list, tail)
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
-- Since we're just inlining and not explicitly handling the closure's
-- environment, this can only support downward closures.  FIXME: Add a
-- check to primval that ensures all variables are defined.
--
-- Currently there doesn't seem to be any risk of violating scoping
-- because the ephemeral lambda values cannot be moved around.
-- I.e. when the variable that binds the closure is in scope, the
-- variables referenced by that closure are as well.
--
-- The takeway here is: keep it simple!  Do not implement higher order
-- forms in this compiler stage, but instead implement those in terms
-- of Scheme macros higher up the abstraction chain.

function class.compile_fun(s, fun)
   return s:parameterize(
      {
         -- Expression is in tail position.
         tail = true,
         -- Don't touch the continuation.  It is hard-coded such that
         -- function return maps to a goto.
      },
      function()
         return
            s:comp(
               block_enter(
                  function(i, arg)
                     s:track_max("nb_args", i)
                     return l(arg, l('arg-ref', i))
                  end,
                  fun.args,
                  l(_(fun.body))))
      end)
end


-- Goto with arguments.
function wrap_args(goto_or_label_expr, args)
   local expr =
      block_enter(
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

-- This is not yet correct.  Problems:

-- 1. It is possible to enter a mutual call network in tail position.
--    When this is true, a new labels form needs to be compiled as
--    well.
--
-- 2. When calling a lambda that is defined inside one of the mutual
--    bodies, it needs to be placed under a deeper labels form.

function class.compile_app(s, fun, args, compiled_cont)

   -- Function instances are specialized to a specific continuation.
   -- We track compilation state for the current continuation inside
   -- the closure data structure.  This will return the label if it
   -- was compiled, or nil if it wasn't.
   local maybe_label = fun.compiled[s.cont]

   -- If the function was not yet compiled, generate a new label to be
   -- used for the goto.
   local label = maybe_label or s:make_var(fun.debug_name)

   -- A call amounts to a goto, with function body inlined at a
   -- specific place if it is not already there.
   local function maybe_compile_label()
      if not maybe_label then
         fun.compiled[s.cont] = label
         local compiled = s:compile_fun(fun)
         local labels = se.car(s.labels)
         assert(labels)
         assert(labels)
         se.push_cdr(l(label, compiled), labels)
      end
   end

   if not compiled_cont then
      -- Tail call
      --
      -- Currently we assume that the labels form is present.  This is
      -- not correct, but will do for now.
      assert(s.cont.fun)
      maybe_compile_label()
      local app = wrap_args(l('goto',label), args)
      trace("APPTAIL", app)
      assert(app)

      return app
   else
      -- Non-tail call: app is compiled as goto + continuation is
      -- applied as goto.
      --
      -- The question is _where_ to hide the bodies.
      --
      -- Compilation happens under a 'labels' form, which behaves
      -- similar to 'letrec' in that all labels are visible from
      -- within the bodies of each of the inlined functions.
      --
      -- Inlined function bodies need to respect scoping rules for
      -- their captured variables, but also for the new labels that
      -- we're introducing, such as the continuation.  This means that
      -- the function bodies need to be compiled below all the
      -- variables they capture, but above the continuation such that
      -- it's label is still visible.
      --
      -- So for non-tail call: always insert a labels form.
      local labels = l('labels')
      s.labels = {labels, s.labels}

      -- Change the current value continuation into a goto that jumps
      -- to the contination body we will insert into the labels form.
      local cont_label = s:make_var(s.cont.var)
      assert(s.cont and (not s.cont.fun))
      s.cont.fun =
         function(val)
            local got = l('goto', cont_label)
            -- Optimize: don't set args when they will be ignored.
            if s.cont.var ~= '_' then
               return wrap_args(got, l(val))
            else
               return got
            end
         end
      se.push_cdr(l(cont_label,compiled_cont),labels)

      -- Now that new labels form is inserted, the fallthrough part of
      -- the labels form is the code that performs the application.
      -- This will compile the call.
      maybe_compile_label()
      local app = wrap_args(l('goto',label), args)

      se.push_cdr(l('_',app), labels)

      trace("APPLABELS", labels)
      return labels

   end
end


function class.comp_bindings(s, bindings_in)

   local function lit_or_ref(thing)
      local typ = se.expr_type(thing)
      if typ == 'var' then return s:ref(thing)
      else return thing end
   end

   local function comp(expr)
      return s:comp(expr)
   end

   return s:parameterize(
      {
         -- These variables are just saved and updated in-place.
         env    = s.env,
         cont   = s.cont,
         labels = s.labels,
      },
      function()
         local parent_cont = s.cont
         local bindings_out_arr = {}
         local function bind(var, val)
            assert(var)
            assert(val)
            ins(bindings_out_arr, l(var, val))
         end


         while not se.is_empty(bindings_in) do

            local binding, rest = unpack(bindings_in)
            local tail = se.is_empty(rest)
            bindings_in = rest

            local var, vexpr = se.unpack(binding, {n=2})

            -- Set the continuation.  The receiver of this is the
            -- primval case at the bottom of the match.  The default
            -- is an ordinary block binding, but it can be modified by
            -- forms to turn it into set! or goto or complex
            -- expressions.
            if tail then
               assert(var == '_')
               s.cont = parent_cont
            elseif var ~= '_' then
               assert(var and var.class == 'var')
               s.cont = var
            else
               s.cont = s:make_var("_")
            end

            trace("CONT",l(s.cont, s.cont.ret ~= nil))

            -- Called before decending into non-primitive expressions.
            local function bind_cps(recurse)
               -- Insert set! continuation if there isn't one already.
               if not s.cont.fun then
                  bind(var, void)
                  trace("CONTSET",vexpr)
                  s.cont.fun = function(val) return l('set!',s.cont,val) end
                  var = '_'
               end
               -- assert(var == '_')
               bind('_', recurse())
            end

            -- Cut the current "program" and compile provided
            -- expression (containing bindings_in) separately.
            local function cut_bindings_in(cont_expr)
               trace("CUTOFF", cont_expr)
               bindings_in = se.empty
               return
                  s:parameterize(
                     {cont = parent_cont},
                     function()
                        return s:comp(cont_expr)
                  end)
            end

            -- Hints contain information that was present in the
            -- original source but is no longer readily available in
            -- the IR.
            local hint = {}
            function hint.letrec(closures)
               log_se_n(closures,"HINT_LETREC:")
               for closure in se.elements(closures) do
                  closure.letrec = closures
               end
            end

            trace("VEXPR", vexpr)

            s.match(
               vexpr,
               {
                  -- Recursive expressions
                  {"(block . ,bindings)", function(m)
                      bind_cps(
                         function()
                            return s:comp_bindings(m.bindings)
                         end)
                  end},
                  {"(if ,c ,t ,f)", function(m)
                      bind_cps(
                         function()
                            return l('if', m.c, s:comp(m.t), s:comp(m.f))
                         end)
                  end},

                  -- Primitive Expressions
                  {"(set! ,var ,val)", function(m)
                      local val = s:ref(m.val)
                      s:set(m.var, val)
                      s:set_debug_name(m.var, val)
                      if m.var.var == '_' then
                         -- This is used e.g. to represent a continuation that
                         -- ignores the return value.
                         bind(var, vold)
                      else
                         local typ = se.expr_type(val)
                         if not ephemeral[typ] then
                            bind(var, val)
                         else
                            s:def(var, val)
                         end
                      end
                  end},
                  {"(lambda ,args ,body)", function(m)
                      -- Definitions are ephemeral.  Bodies will be compiled
                      -- when they are referenced by 'app'.
                      local fun = {
                         class = 'closure',
                         args = m.args,
                         body = m.body,
                         env = s.env,
                         compiled = {},
                         iolist = closure_iolist,
                      }
                      s:set_debug_name(s.cont, fun)
                      s:def(var, fun)
                      -- FIXME: Insert a labels form here as well?
                  end},

                  -- Application (ephemeral, hint, primitive, closure)
                  {"(app ,fun . ,args)", function(m)
                      local i=0
                      local fun = s:ref(m.fun)
                      trace("APP",l(m.fun, fun))
                      if type(fun) == 'function' then
                         -- Ephemeral functions are evaluated.  This
                         -- is currently only used for lib-ref, which
                         -- is given a symbol and returns a prim.
                         local vals = se.map(lit_or_ref, m.args)
                         local rv = fun(unpack(l2a(vals)))
                         -- Only ephemeral.
                         s:def(var, rv)
                      else
                         assert(fun.class)
                         if fun.class == 'prim' then
                            if fun.name == 'hint' then
                               -- Hints are encoded as a regular
                               -- function call such that they do not
                               -- have any syntactic significance.
                               -- However, the information they
                               -- provide can lead to compilation
                               -- actions.
                               local hint_name, hint_args = unpack(m.args)
                               local hint_fun = hint[hint_name.expr]
                               if hint_fun then
                                  hint_fun(se.map(lit_or_ref, hint_args))
                               else
                                  log("WARNING: unknown hint " .. hint_name)
                               end
                            else
                               -- Primitives are compiled
                               bind(var, {fun, m.args})
                            end
                         elseif fun.class == 'closure' then
                            -- Closures are compiled
                            local compiled_cont = nil
                            if not tail then
                               -- Compile the continuation = remainder of the bindings form.
                               compiled_cont =
                                  cut_bindings_in({'block',{l(var,l('arg-ref', 0)),bindings_in}})
                            end
                            -- Compile the call (goto) + continuation if any.
                            local app = s:compile_app(fun, m.args, compiled_cont)
                            bind('_', app)
                         else
                            error("bad func class '" .. fun.class .. "'")
                         end
                      end
                  end},

                  -- Primitive value: return to continuation.
                  {",primval", function(m)
                      -- FIXME: Check all references.  We are inlining
                      -- closures assuming their closed variables are
                      -- bound.
                      s:def(var, m.primval)
                      local typ = se.expr_type(m.primval)
                      if not ephemeral[typ] then
                         if  s.cont.fun then
                            trace("PVCONT",m.primval)
                            bind(var, s.cont.fun(m.primval))
                         else
                            -- This is an ordinary block binding.
                            trace("PVRET",m.primval)
                            bind(var, m.primval)
                         end
                      end
                  end},

               })
         end
         return {'block',a2l(bindings_out_arr)}
   end)
end




-- Compiler is contained in comp_bindings.  This is just a trampoline
-- that wraps a block if necessary.
function class.comp(s, expr)
   trace("COMP",expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             return s:comp_bindings(m.bindings)
         end},
         {",other", function(m)
             return s:comp_bindings(l(_(m.other)))
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

   -- Top continuation
   local ret = s:make_var()
   ret.fun = function(val) return l('return',val) end

   -- The 'var' parameter contains the continuation, which is a
   -- variable that will receive the value of the computation.  If
   -- s.cont.fun is defined, it encodes other behavior such as 'set!',
   -- 'goto' or 'return'.
   s.cont = ret

   -- Tracks the maximum of arg-ref
   s.nb_args = -1

   -- Tracks the list of labels forms.  Needs to be initialized with
   -- toplevel one.
   local labels = l('labels')
   s.labels = l(labels)

   return s.match(
      expr,
      {{"(lambda (,lib_ref) ,body)", function(m)
           -- The outer form is a lambda that binds the library lookup
           -- function, which gets passed the names of all the free
           -- variables in the original scheme code.
           local function lib_ref(name)
              return {
                 class = 'prim',
                 name = name.expr,
                 iolist = {"prim:",name.expr}
              }
           end
           s:def(m.lib_ref, lib_ref)

           local c_body = s:comp(m.body)

           se.push_cdr(l('_', c_body), labels)

           return l('block',
                    _(l('alloc_args', 1 + s.nb_args)),
                    _(labels))
      end}})
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
