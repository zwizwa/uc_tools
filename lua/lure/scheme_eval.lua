-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives through main expression's 'base-ref' parameter.
-- . Simpler to implement this in Lua, using pattern matching library.


local se       = require('lure.se')
local se_match = require('lure.se_match')
local comp     = require('lure.comp')

local function trace(tag, expr)
   -- log_se_n(expr, tag .. ":")
end


local l2a       = se.list_to_array
local l         = se.list
local is_empty  = se.is_empty
local is_pair   = se.is_pair
local empty     = se.empty
local car       = se.car
local cdr       = se.cdr
local map       = se.map
local zip       = se.zip
local se_unpack = se.unpack
local length    = se.length

local class = {}

local void = "#<void>"

local function ifte(c,t,f)
   if c then return t else return f end
end

-- These operate on s.env
class.def       = comp.def
class.find_cell = comp.find_cell
class.ref       = comp.ref
class.set       = comp.set

function frame(var, program, env)
   return {class = 'frame', var = var, program = program, env = env }
end

function class.base_ref(s,name)
   assert(type(name) == 'string')
   local fun = s.prim[name]
   if fun == nil then
      error("primitive '" .. name .. "' not defined")
   end
   trace("PRIM",name)
   return fun
end


-- Perform a closure call.


function class.app_closure(s, fun, vals)
   local is_tail = is_empty(s.rest)
   s:push_if_rest()
   trace("APPLY",l(fun.args, vals))
   s.env = fun.env
   zip(function(var,val) s:def(var,val) end, fun.args, vals)
   s.expr = fun.body
end

-- The evaluation context needs to be saved and restored when
-- evaluating non-tail calls.
function class.get_k(s)
   trace("PUSH",s.env)
   local frame = {
      env  = s.env,
      var  = s.var,
      rest = s.rest,
      class = 'kframe',
   }
   return {frame, s.k}
end

function class.push(s)
   s.k = s:get_k()
   s.var = '_'
   s.rest = empty
end

function class.pop(s)
   local frame = car(s.k)
   s.k     = cdr(s.k)
   s.env   = frame.env
   s.var   = frame.var
   s.rest  = frame.rest
   trace("POP",s.env)
end

-- This is for app and block descent: when there is a rest expression,
-- context needs to be saved, otherwise eval can happen in-place.
function class.push_if_rest(s)
   if not is_empty(s.rest) then
      s:push()
   else
      assert(s.var == '_')
   end
end

-- Move on to the next instruction in s.rest
function class.advance(s)
   assert(not is_empty(rest))
   local binding
   binding, s.rest = unpack(s.rest)
   s.var, s.expr = se_unpack(binding, {n = 2})
end

-- Value return for primtive data.
function class.ret(s,val)
   if (is_empty(s.rest)) then
      assert(s.var == '_')
      s:pop()
   end
   if (s.var ~= '_') then
      trace("DEF", l(s.var, val))
      s:def(s.var, val)
   else
      trace("IGN", l(s.var, val))
   end
   s:advance()
end

-- Application of thing to values is implemented abstractly based on
-- the class tag in the object.  Currently there are two:

class.app = {}
-- Ordinary closure application.
class.app.closure = class.app_closure
-- Machine operation extensions.  See e.g. call/cc prim.
function class.app.mop(s, mop, vals)
   mop.mop(s, vals)
end

function class.k_fun(s, k_snap)
   k_snap = k_snap or s:get_k()
   return function(val)
      trace("KFUN", val)
      s.k = k_snap
      s:pop()
      return val
   end
end
-- Example mop: call-with-current-continuation
function class.callcc(s, args)
   local fun = se.unpack(args, {n=1})
   assert(fun and fun.class == 'closure')
   -- Snap cont + wrap it as a primitive function that restores it.
   local k_fun = s:k_fun()
   -- Apply the closure
   s:app_closure(fun, l(k_fun))
end

function class.reset(s)

   -- Machine state is stored in s to allow reuse and extension.

   -- The lexical environment is implemented as a flat list.  Slow but
   -- convenient.  Stored as s.env to allow def, ref, set methods.
   s.env = empty

   -- The variable that takes the value of the current expression.
   s.var = '_'

   -- The remainder of the current block.
   s.rest = empty

   -- Top level contination falls into the halt instruction.
   local k_var = {
      class  = 'var',
      iolist = 'k_var',
      unique = 'k_var'
   }
   local top_kframe = {
      class = 'kframe',
      env = s.env,
      var = k_var,
      rest = l(l('_',l('halt',k_var)))
   }
   s.k = { top_kframe, empty }

   -- Register top continuation as a primitive
   s.prim['abort'] = s:k_fun()

end


function class.eval(s, top_expr)

   -- Top level expression coming out of scheme_frontend is a lambda
   -- that defines the linker for all free variables present in the
   -- original source.  We evaluate this lambda expression manually to
   -- insert that binding.
   s.match(
      top_expr,
      {{'(lambda (,base_ref) ,expr)',
        function(m)
           assert(m.base_ref.class == 'var')
           s:def(m.base_ref, function(sym) return s:base_ref(sym) end)
           s.expr = m.expr
   end}})

   -- Primitive value: literal or variable referenece.
   local function lit_or_ref(thing)
      trace("REF",l(thing,s.env))
      local typ = type(thing)
      if typ ~= 'table' then return thing end

      local class = thing.class
      assert(class)
      if 'var' == class then
         return s:ref(thing)
      elseif 'expr' == class then
         return thing.expr
      elseif 'void' == class then
         return void
      elseif 'prim' == class then
         -- The scheme_sm output IR uses this to quote primitive names
         -- as data.  We map it to our prims here.
         return s:base_ref(thing.name)
      else
         error("lit_or_ref, bad class '" .. class .. "'")
      end
   end

   -- Main loop
   local rv
   while rv == nil do
      trace("EVAL", s.expr)

      s.match(
         s.expr,
         {
            {"(halt ,rv)", function(m)
                rv = lit_or_ref(m.rv)
            end},
            {"(if ,cond ,iftrue ,iffalse)", function(m)
                local cond = lit_or_ref(m.cond)
                s.expr = ifte(cond, m.iftrue, m.iffalse)
            end},
            {"(block (_ ,expr))", function(m)
                s.expr = m.expr
            end},
            {"(block (,var ,expr))", function(m)
                error("last expression in 'block' is bound: '" .. m.var .. "'")
            end},
            {"(block)", function(m)
                s:ret(void)
            end},
            {"(block . ,bindings)", function(m)
                s:push_if_rest()
                s.rest = m.bindings
                s:advance()
            end},
            {"(set! ,var ,val)", function(m)
                s:set(m.var, lit_or_ref(m.val))
                s:ret(void)
            end},
            {"(lambda ,args ,body)", function(m)
                trace("LAMBDA",l(m.args, m.body))
                s:ret({args = m.args, body = m.body, env = s.env, class = 'closure'})
            end},
            {"(app ,fun . ,args)", function(m)
                local fun = lit_or_ref(m.fun)
                local vals = map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   local rv = fun(unpack(l2a(vals)))
                   if rv == nil then rv = void end
                   trace("PRIM_EVAL", rv)
                   s:ret(rv)
                elseif type(fun) == 'table' then
                   local class = fun.class  -- closure, mop
                   local app = s.app[class]
                   s.expr = nil
                   app(s, fun, vals)
                else
                   log_se_n(s.env,  "ENV:")
                   log_se_n(s.expr, "EXPR:")
                   error("bad fun type '" .. type(fun) .. "'")
                end
            end},
            {"(hint ,fun . ,args)", function(m)
                s:advance()
            end},
            {",other", function(m)
                if is_pair(m.other) then
                   error('bad form')
                else
                   local v = lit_or_ref(m.other)
                   assert(v ~= nil)
                   s:ret(v)
                end
            end},
         })
   end

   return rv

end




function class.new(prim_base)
   local prim = { }
   setmetatable(prim, {__index = prim_base})
   local s = { match = se_match.new(), prim = prim }
   setmetatable(s, {__index = class})
   -- call-with-current-continuation, implemented as a mop
   prim['call/cc'] = { class = 'mop', mop = s.callcc }
   s:reset(s)
   return s
end

return class
