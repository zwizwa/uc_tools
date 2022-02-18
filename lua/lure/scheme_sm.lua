-- Compiler from block language to State Machine.  This has slightly
-- modified structure that cannot represent all of Scheme.  The two
-- main differences are:
--
-- 1. All function applications are inlined, with support for loops.
--
-- 2. Downward closures are allowed.
--

-- FIXME: Make sure that variables that show up in code are not
-- ephemeral.  E.g. don't just drop things to void.


local se       = require('lure.se')
local comp     = require('lure.comp')
local se_match = require('lure.se_match')
local frontend = require('lure.scheme_frontend')

local ins = table.insert
local l2a = se.list_to_array
local a2l = se.array_to_list
local l = se.list

local car = se.car


local class = {}

class.parameterize = comp.parameterize
local void = frontend.void

local function _trace(tag, expr)
   log_se_n(expr, tag .. ":")
end
local function trace(tag, expr)
   _trace(tag, expr)
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

-- Compile a block with some 0-indexed statements at the beginning.
-- This is used for arg-ref and set-arg!.
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
-- environment, this can only support downward closures.
--
-- FIXME: Add a check to primval that ensures all variables are
-- defined.
--
-- The takeway here is: moving lambda bodies around is necessary but
-- not trivial.  Keep it simple!  Do not implement higher order forms
-- in this compiler stage, but instead implement those in terms of
-- Scheme macros higher up the abstraction chain.

function class.compile_fun(s, fun)
   fun = s:rename_closure(fun)
   return s:parameterize(
      { tail = true },
      function()
         local body =
            s:comp(
               block_enter(
                  function(i, arg)
                     s:track_max("nb_args", i)
                     return l(arg, l('arg-ref', i))
                  end,
                  fun.args,
                  l(_(fun.body))))
         return body
      end)
end

-- goto / label fallthrough with arguments.
function wrap_set_args(goto_or_label_expr, args)
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


-- Create a new closure with renamed variables.  This does more than
-- alpha conversion: it also renames new variables introduced in the
-- body to prepare it for inlining.
function class.rename_closure(s, fun)
   local env_map = {}
   local renamed = {}
   for binding in se.elements(fun.env) do
      env_map[car(binding)] = true
   end
   local function rename(var)
      if env_map[var] then return var end
      local var1 = renamed[var]
      if var1 ~= nil then return var1 end
      var1 = s:make_var(var.name)
      renamed[var] = var1
      return var1
   end
   local fun1 = s:make_closure(
      se.fmap('var',rename,fun.args),
      se.fmap('var',rename,fun.body),
      fun.env)
   trace("RENAME_OLD:", l(fun.args, fun.body))
   trace("RENAME_NEW:", l(fun1.args, fun1.body))
   return fun1
end

function class.make_closure(s, args, body, env)
   env = env or s.env
   return {
      class = 'closure',
      args = args,
      body = body,
      env = env,
      compiled = {},
      iolist = closure_iolist
   }
end

-- For debugging, closures have names determined from the initial
-- binding or the last assigment.  This is a hack but works for letrec
-- output.
function class.set_debug_name(s, var, val)
   local typ = se.expr_type(val)
   if typ == 'closure' then
      val.debug_name = var.var
   end
end

function class.push_to_current_labels(s, label, body_expr)
   -- FIXME: This is currently a stack.  Is that really necessary?
   assert(s.labels)
   local current_labels = car(s.labels)
   se.push_cdr(l(label, body_expr), current_labels)
end


function class.compile_app(s, fun, args, compiled_cont)

   -- This is tricky to define properly... The essential piece of
   -- information we need is whether this call enters a
   -- not-yet-compiled mutually recursive call network, or whether it
   -- is a call that is inside such a network.
   --
   -- We disambiguate it like this:
   --
   -- 1. If fun is mutually recursive (information recovered via
   --    letrec hint), then compile _all_ instances in the letrec
   --    network if it is not yet compiled.
   --
   -- 2. We know whether a function is compiled by tracking its label
   --    _specialized to the current continuation_.
   --

   -- Function instances are specialized to a specific continuation.
   -- We track compilation state for the current continuation inside
   -- the closure data structure.  This will return the label if it
   -- was compiled, or nil if it wasn't.
   local maybe_label = fun.compiled[s.cont]
   local already_compiled = maybe_label ~= nil

   -- If the function was not yet compiled, generate a new label to be
   -- used for the goto.
   local label = maybe_label or s:make_var(fun.debug_name)

   -- Compile the current function and all its letrec siblings.
   local function compile()
      local funs = l(fun)
      if fun.letrec then
         funs = fun.letrec
      end
      -- Mark them first to prevent infinite compilation loop, as we
      -- will re-enter compile_app() here.  We have the label for fun
      -- already...
      fun.compiled[s.cont] = label
      -- But the other ones need to be generated.
      for other in se.elements(funs) do
         if other ~= fun then
            other.compiled[s.cont] = s:make_var(other.debug_name)
         end
      end
      for f in se.elements(funs) do
         local f_compiled = s:compile_fun(f)
         local f_label = f.compiled[s.cont]
         assert(f_label)
         s:push_to_current_labels(f_label, f_compiled)
      end
   end

   local function insert_labels()
      local labels = l('labels')
      s.labels = {labels, s.labels}
      return labels
   end

   -- If everything is compiled in the proper context, the call
   -- amounts to a goto + setting of argument registers.
   local app = wrap_set_args(l('goto',label), args)


   if not compiled_cont then
      -- Tail call
      --
      if not already_compiled then

         -- If the function is not yet compiled for the current
         -- contination, we need to insert a labels form to host
         -- (potential) mutually recursive functions.
         local labels = insert_labels()
         compile()
         -- Save it.  Here we assume this will be the "fall through"
         -- label, i.e. nobody will push to this lables form after
         -- this app.  This should be true beacuse all the compilation
         -- and pushing happened before.
         s:push_to_current_labels('_', app)
         return labels
      else
         -- Already compiled, this is a call inside the network which
         -- is just a goto.
         return app
      end

      assert(s.cont.fun)

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
      local labels = insert_labels()

      -- Change the current value continuation into a goto that jumps
      -- to the contination body we will insert into the labels form.
      local cont_label = s:make_var(s.cont.name)
      assert(s.cont and (not s.cont.fun))
      s.cont.fun =
         function(val)
            local goto_cont = l('goto', cont_label)
            -- Optimize: don't set args when they will be ignored.
            if s.cont.name ~= '_' then
               return wrap_set_args(goto_cont, l(val))
            else
               return goto_cont
            end
         end
      s:push_to_current_labels(cont_label, compiled_cont)

      -- Now that new labels form is inserted we can compile
      -- everything that is relative to this call.
      compile()

      -- The fallthrough part of the labels form is the code that
      -- performs the application.  This is just a goto.
      s:push_to_current_labels('_',app)

      return labels

   end
end

local function is_var(var)
   return se.expr_type(var) == 'var'
end

-- The compile-time ephemeral environment only needs to indicate that
-- variables that are in the output are in scope.
local runtime = {class = 'runtime'}
class.runtime = runtime

function class.comp_bindings(s, bindings_in)

   local function lit_or_ref(thing)
      local typ = se.expr_type(thing)
      if typ == 'var' then
         local val = s:ref(thing)
         return val
      else
         return thing
      end
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


         while not se.is_empty(bindings_in) do

            local binding, rest = unpack(bindings_in)
            local tail = se.is_empty(rest)
            bindings_in = rest

            local var, vexpr = se.unpack(binding, {n=2})

            -- Our main task here is to 1. collect ephemeral bindings
            -- such as lambda expressions into the current s.env, and
            -- 2. to build the output 'block' form, which amounts to
            -- translating each binding form into something that can
            -- be implemented in a language like C.

            -- What survives is primitive non-tail bindings (values
            -- and primitive function calls).  Most of the rest is
            -- modified by chainging the continuation in some way, to
            -- either an explicit return, goto + arg reg set, or an
            -- explicit set!  to another variable.

            -- Start out by setting the current continuation based on
            -- the parent continuation and the position in the
            -- bindings form.

            -- A contination starts out with fun == false, indicating
            -- a primitive binding.  This default is modified where
            -- necessary by setting s.cont.fun to a function that
            -- takes a primitive expression, to implement e.g. goto,
            -- set!  continations.
            if tail then
               assert(var == '_')
               s.cont = parent_cont
            elseif var ~= '_' then
               assert(var and var.class == 'var')
               s.cont = s:make_cont(var.var, false)
            else
               s.cont = s:make_cont('_', false)
            end



            trace("CONT",l(tail, var, s.cont, s.cont.fun))

            -- Consistency check: if there is a custom continuation
            -- handler, the binding here is meaningless and should be
            -- ignored.
            local function check_cont(var, dbg)
               if s.cont.fun and var ~= '_' then
                  trace("VAR",l(var, dbg or l()))
                  error('s.cont.fun and var')
               end
            end

            -- Insert a primitive binding in the outgoig bindings
            -- form.  Invoke the continuation if it is defined.  This
            -- always goes in the default var position.
            local function ins_prim(val)
               check_cont(var, l('prim',val))
               if s.cont.fun then
                  val = s.cont.fun(val)
               end
               ins(bindings_out_arr, l(var, val))
               if var ~= '_' then
                  s:def(var, runtime)
               end
               var = '_'
            end

            -- Basic form: save variable binding to variable.
            local function ins_subexpr(val)
               assert(var)
               assert(val)
               check_cont(var, l('subexpr',val))
               ins(bindings_out_arr, l(var, val))
               if var ~= '_' then
                  s:def(var,runtime)
               end
               var = '_'
            end

            -- Called before decending into non-primitive expressions.
            -- This is for block forms that cannot be implemented as
            -- expressions, and need an explicit set (if they don't
            -- have some other continuation handler already).
            local function ins_retval_subexpr(recurse, dbg)
               if not s.cont.fun then
                  if var ~= '_' then
                     -- Define undefined variable and install the set! continuation.
                     local cont_var = var  -- var is muted, so deref before capture
                     ins_subexpr(void)
                     s.cont.fun = function (val)
                        if dbg then dbg(val) end
                        return l('set!',cont_var,val)
                     end
                  else
                     -- The subexpression value is ignored. Just recurse.
                  end
               else
                  -- Continuation is already set.
                  assert(var == '_')
               end
               ins_subexpr(recurse())
            end

            -- Compile the rest of the bindings as a 1-arg function
            -- that can be installed in a labels form.  This is passed
            -- to compile_app.
            local function compile_cont()
               local cont_expr = {'block',{l(var,l('arg-ref', 0)),bindings_in}}
               var = '_'
               bindings_in = se.empty
               trace("COMPILE_CONT", cont_expr)
               return
                  s:parameterize(
                     { cont = parent_cont },
                     function()
                        return s:comp(cont_expr)
                     end)
            end

            -- Hints contain information that was present in the
            -- original source but is no longer readily available in
            -- the IR.
            local hint = {}
            -- We currently rely on this letrec hint to correctly
            -- compile all letrec siblings at once when we encounter
            -- one.  That information can in principle be recovered
            -- through analysis, but it's much easier to let the
            -- letrec macro insert the hint, since it has this
            -- information centralized before it gets spread out.
            function hint.letrec(closures)
               _trace("HINT_LETREC",closures)
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
                      ins_retval_subexpr(
                         function()
                            return s:comp_bindings(m.bindings)
                         end)
                  end},
                  {"(if ,c ,t ,f)", function(m)
                      ins_retval_subexpr(
                         function()
                            return l('if', m.c, s:comp(m.t), s:comp(m.f))
                         end)
                  end},

                  -- Primitive Expressions
                  {"(set! ,var ,val)", function(m)
                      -- Deref for ephemeral use + check that
                      -- variables obey scoping rules
                      local val_var = s:ref(m.var)
                      local val = s:ref(m.val)
                      s:set(m.var, val)
                      s:set_debug_name(m.var, val)
                      if m.var.var == '_' then
                         -- This is used e.g. to represent a continuation that
                         -- ignores the return value.
                         ins_prim(vold)
                      else
                         local typ = se.expr_type(val)
                         if not ephemeral[typ] then
                            ins_prim(l('set!', m.var, m.val))
                         end
                      end
                  end},
                  {"(lambda ,args ,body)", function(m)
                      -- Definitions are ephemeral and do not show up
                      -- in output code.  Bodies will be compiled when
                      -- they are referenced by 'app'.
                      local fun = s:make_closure(m.args, m.body, s.env)
                      s:set_debug_name(var, fun)
                      s:def(var, fun)
                      -- FIXME: Insert a labels form here as well?
                  end},
                  {"(hint ,name . ,args)", function(m)
                      local vals = se.map(lit_or_ref, m.args)
                      local hint_fun = hint[m.name.expr]
                      _trace("HINT",l(m.name, vals))
                      if hint_fun then
                         hint_fun(se.map(lit_or_ref, vals))
                      else
                         log("WARNING: unknown hint " .. hint_name)
                      end
                  end},
                  -- Application (ephemeral, hint, primitive, closure)
                  {"(app ,fun . ,args)", function(m)
                      local i=0
                      local fun = s:ref(m.fun)
                      -- Deref for possible ephemeral use.  This also
                      -- checks that the references obey normal
                      -- scoping rules.
                      local val = lit_or_ref(m.primval)
                      -- trace('PRIMVAL_DEFINED', val)

                      local vals = se.map(lit_or_ref, m.args)
                      trace("APP",l(m.fun, fun))

                      if type(fun) == 'function' then
                         -- Ephemeral functions are evaluated.  This
                         -- is currently only used for lib-ref, which
                         -- is given a symbol and returns a prim.
                         local rv = fun(unpack(l2a(vals)))
                         s:def(var, rv)
                         local typ = se.expr_type(rv)
                         if not ephemeral[typ] then
                            ins_prim(rv)
                         end
                      else
                         assert(fun.class)
                         if fun.class == 'prim' then
                            -- Primitives are compiled
                            ins_prim({fun, m.args})

                         elseif fun.class == 'closure' then

                            local already_compiled = fun.compiled[s.cont] ~= nil

                            -- Support for partial evaluation
                            if not already_compiled then
                               -- Try partial evaluation, see if it
                               -- produces another closure.  If so
                               -- then bind as ephemeral value.
                            end

                            -- Not recursive? Can be inlined.
                            if not already_compiled and not fun.letrec then
                               local inlined_bindings =
                                  se.append(se.zip(l, fun.args, m.args), l(_(fun.body)))
                               trace("INLINED_BINDINGS", inlined_bindings)
                               ins_retval_subexpr(
                                  function()
                                     return s:comp_bindings(inlined_bindings)
                                  end,
                                  function(rv)
                                     _trace("INLINED_RV", rv)
                                  end
                               )
                            else
                               -- Closures defined by letrec are potentially recursive.
                               local compiled_cont = (not tail) and compile_cont()
                               local app = s:compile_app(fun, m.args, compiled_cont)
                               ins_subexpr(app)
                            end
                         else
                            _trace("BAD_FUN", m.fun)
                            if fun.class == 'runtime' then
                               -- This happens when a function value is
                               -- not ephemeral.
                               error("function not ephemeral")
                            else
                               error("bad fun class '" .. fun.class .. "'")
                            end
                         end
                      end
                  end},

                  -- Primitive value: return to continuation.
                  {",primval", function(m)
                      -- Check that the reference obeys normal scoping
                      -- rules.
                      local val = lit_or_ref(m.primval)
                      -- _trace('PRIMVAL_DEFINED', val)
                      local typ = se.expr_type(m.primval)
                      if not ephemeral[typ] then
                         ins_prim(m.primval)
                      else
                         s:def(var, m.primval)
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

function class.make_cont(s, name, fun)
   assert(fun ~= nil)
   return {
      class = 'cont',
      name = name,
      fun = fun,
   }
end

class.special = {
   ['true']  = { class = 'expr', expr = true,  iolist = '#t' },
   ['false'] = { class = 'expr', expr = false, iolist = '#f' },
}

function class.compile(s,expr)
   -- Prefix needs to be different from what is used in the frontend,
   -- so we don't clash.
   s.symbol_prefix = "l"

   -- Initialize gensym counter
   s.nb_sym = 0

   -- Top environment is empty.
   s.env = se.empty

   -- Top continuation
   s.cont = s:make_cont(
      'ret',
      function(expr)
         -- local ret_var = {class = 'var', iolist = 'ret_var'}
         -- return l('block', l(ret_var, expr), l('_', l('return', ret_var)))
         return l('return', expr)
      end)

   -- Tracks the maximum of arg-ref
   s.nb_args = -1

   -- Tracks the list of labels forms.  Needs to be initialized with
   -- toplevel one.
   -- local labels = l('labels')
   -- s.labels = l(labels)
   s.labels = se.empty

   return s.match(
      expr,
      {{"(lambda (,lib_ref) ,body)", function(m)
           -- The outer form is a lambda that binds the library lookup
           -- function, which gets passed the names of all the free
           -- variables in the original scheme code.
           local function lib_ref(name)
              local special = s.special[name.expr]
              local val
              if special ~= nil then
                 -- log_se_n(special,"SPECIAL:")
                 val = special
              else
                 val = {
                 class = 'prim',
                 name = name.expr,
                 iolist = {"prim:",name.expr}
                 }
              end
              -- log_se_n(l(name,val), "LIB_REF:")
              return val
           end
           s:def(m.lib_ref, lib_ref)

           local c_body = s:comp(m.body)

           -- se.push_cdr(l('_', c_body), labels)

           return l('block',
                    _(l('alloc_args', 1 + s.nb_args)),
                    -- _(labels)
                    _(c_body)
                    )
      end}})
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
