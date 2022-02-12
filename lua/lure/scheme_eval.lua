-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives through main expression's 'base-ref' parameter.
-- . Simpler to implement this in Lua, using pattern matching library.


-- Note that there are 2 kinds of continuations: the one that is used
-- to save context to evaluate a non-tail closure call, and a 'light'
-- one that occurs when nesting blocks, which only needs to save the
-- current 'variable continuation', which can be done by rewriting the
-- code block.  ( Which is essentially what 'scheme_flatten' pass
-- does. )

local se       = require('lure.se')
local se_match = require('lure.se_match')
local comp     = require('lure.comp')

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
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
   if not fun then
      error("primitive '" .. name .. "' not defined")
   end
   trace("PRIM",name)
   return fun
end



function class.eval(s, top_expr)

   -- The lexical environment is implemented as a flat list.  Slow but
   -- convenient.  Stored as s.env to allow def, ref, set methods.
   s.env = empty

   -- The rest of the evaluation context can be local variables: the
   -- stack / contination list k, the variable that takes the value of
   -- the current expression, and the rest of the 'program', which is
   -- a 'block' form without the tag.

   local k = empty
   local var = '_'
   local rest = empty

   -- Top level expression coming out of scheme_frontend is a lambda
   -- that defines the linker for all free variables present in the
   -- original source.  We evaluate this lambda expression manually to
   -- insert that binding...
   local ret_var = { class = 'var', iolist = 'ret_var' }
   local expr =
      s.match(
         top_expr,
         {{'(lambda (,base_ref) ,expr)',
           function(m)
              assert(m.base_ref.class == 'var')
              s:def(m.base_ref, function(sym) return s:base_ref(sym) end)
              -- ... and install a trampoline that binds the remainder
              -- of the expression to a variable before breaking the
              -- loop.
              return l('block',l(ret_var, m.expr),l('_',l('halt')))
      end}})

   -- The evaluation context needs to be saved and restored when
   -- evaluating non-tail calls.
   local function get_k()
      trace("PUSH",s.env)
      local frame = {
         env  = s.env,
         var  = var,
         rest = rest,
         class = 'kframe',
      }
      return {frame, k}
   end
   local function push()
      k = get_k()
      var = '_'
      rest = empty
   end
   local function pop()
      local frame = car(k)
      k     = cdr(k)
      s.env = frame.env
      var   = frame.var
      rest  = frame.rest
      trace("POP",s.env)
   end

   -- Perform a closure call.
   local function app(fun, vals)

      local is_tail = is_empty(rest)

      -- Primitives handled elsewhere.
      assert('function' ~= type(fun))

      -- Only push when not a tail call.
      if not is_tail then
         push()
      else
         assert(var == '_')
      end

      trace("APPLY",l(fun.args, vals))
      s.env = fun.env
      zip(function(var,val) s:def(var,val) end, fun.args, vals)
      expr = fun.body

      -- The evaluation context is now empty, containing just the body
      -- and environment of the closure, a single var '_', and no
      -- remaining code in rest.  If that state is reached for a
      -- primitive evaluation, the context is popped.

   end

   -- Value return for primtive data.
   local function ret(val)

      trace("RET",l(val,rest))

      -- If we're at the end of the line, pop the contination.
      if (is_empty(rest)) then
         -- Structural constraint for last binding in block.
         assert(var == '_')
         pop()
      end

      -- Bind variable and continue executing.
      if (var ~= '_') then
         trace("DEF", l(var, val))
         s:def(var, val)
      else
         trace("IGN", l(var, val))
      end

      -- There is always more code to execute.  Note that the 'halt'
      -- instruction inserted by the trampoline will stop the machine
      -- before running off the end.
      assert(not is_empty(rest))
      local binding
      binding, rest = unpack(rest)
      var, expr = se_unpack(binding, {n = 2})
   end

   local op_as_prim = { class = 'op_as_prim' }

   s.prim['call/cc'] = function(fun)
      -- If it's not a closure, maybe wrap it?
      assert(fun and fun.class == 'closure')
      -- Push continuation so we can store it and switch to function
      -- context.
      push()
      s.env = fun.env
      expr  = fun.body
      -- Wrap the continuation object in a function that behaves as a
      -- primitive.
      local k_saved = k
      local function k_fun(val)
         trace("KFUN", val)
         k = k_saved ; pop()
         assert(val)
         return val
      end
      assert(length(fun.args) == 1)
      s:def(fun.args[1], k_fun)
      return op_as_prim
   end

   -- Primitive value: literal or variable referenece.
   local function lit_or_ref(thing)
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
   local halted = false
   while not halted do
      trace("EVAL", expr)

      s.match(
         expr,
         {
            {"(halt)", function(m)
                halted = true
            end},
            {"(if ,cond ,iftrue ,iffalse)", function(m)
                expr = ifte(lit_or_ref(m.cond), m.iftrue, m.iffalse)
            end},
            {"(block (_ ,expr))", function(m)
                expr = m.expr
            end},
            {"(block (,var ,expr))", function(m)
                error("last expression in 'block' is bound: '" .. m.var .. "'")
            end},
            {"(block (,var ,expr) . ,rest)", function(m)
                local binding = l(var, {'block', m.rest})
                rest = {binding,rest}
                var  = m.var
                expr = m.expr
            end},
            {"(block)", function(m)
                ret(void)
            end},
            {"(set! ,var ,val)", function(m)
                s:set(m.var, lit_or_ref(m.val))
                ret(void)
            end},
            {"(lambda ,args ,body)", function(m)
                trace("LAMBDA",l(m.args, m.body))
                ret({args = m.args, body = m.body, env = s.env, class = 'closure'})
            end},
            {"(app ,fun . ,args)", function(m)
                local fun = lit_or_ref(m.fun)
                local vals = map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   local rv = fun(unpack(l2a(vals)))
                   if rv ~= op_as_prim then
                      if rv == nil then rv = void end
                      trace("PRIM_EVAL", rv)
                      ret(rv)
                   else
                      -- Function has manipulated the machine, so
                      -- don't do anything here
                   end
                else
                   app(fun, vals)
                end
            end},
            {",other", function(m)
                if is_pair(m.other) then
                   error('bad form')
                else
                   local v = lit_or_ref(m.other)
                   assert(v)
                   ret(v)
                end
            end},
         })
   end

   return s:ref(ret_var)

end


function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
