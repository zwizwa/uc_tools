-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives through main expression's 'base-ref' parameter.
-- . Simpler to implement this in Lua, using pattern matching library.


local se       = require('lure.se')
local se_match = require('lure.se_match')
local comp     = require('lure.comp')

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
end


local l2a = se.list_to_array
local l = se.list
local is_empty = se.is_empty


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

-- The 'program' type used throughout is a list of bindings from the
-- 'block' form.



function class.eval_loop(s, top_expr)


   -- The lexical environment is implemented as a flat list.  This is
   -- slow, but very convenient for analysis.  This is stored in the
   -- object to allow reuse of def, ref, set.
   s.env = se.empty

   -- The other state contains the current variable to be bound, the
   -- current expression under evaluation, the rest of the program,
   -- and the continuation.
   local ret_var = { class = 'var' }
   local var  = ret_var
   local expr = top_expr
   local k    = se.empty
   local rest = l(l('_', l('halt')))

   -- This context needs to be saved and restored when evaluating
   -- non-tail calls.
   local function push()
      local frame = {
         env = s.env,
         var = s.var,
         rest = s.rest,
      }
      k = {frame, k}
   end
   local function pop()
      local frame = se.car(k)
      k = se.cdr(k)
      s.env = frame.env
      var = frame.var
      rest = frame.rest
   end

   -- Top level expression is a lambda that defines the linker for all
   -- free variables present in the original source.  We evaluate that
   -- manually to insert the binding.
   expr =
      s.match(
         expr,
         {{'(lambda (,base_ref) ,expr)',
           function(m)
              assert(m.base_ref.class == 'var')
              s:def(m.base_ref, function(sym) return s:base_ref(sym) end)
              return m.expr
      end}})

   -- Value return for primtive data.
   local function ret(val)
      -- If we're at the end of the line, pop the contination: the
      -- binding is stored in the enclosing program.
      if (is_empty(rest)) then
         -- Structural constraint for last binding in block.
         assert(var == '_')
         pop()
      end
      -- FIXME: Is this ever the case?  I don't think so due to tail
      -- call elimination.
      assert(not is_empty(rest))
      -- Bind variable and continue executing.
      if (var ~= '_') then
         s:def(var, val)
      end
      local binding
      binding, rest = unpack(rest)
      var, expr = se.unpack(binding, {n = 2})
   end


   -- Primitive value: literal or variable referenece.
   local function lit_or_ref(thing)
      if type(thing) ~= 'table' then return thing end
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

   -- Replace current context with that of the closure.  Is preceeded
   -- by push this needs to be rpush a continuation if not in tail pos
   local function app(fun, vals)
      -- Primitives handled elsewhere.
      assert('function' ~= type(fun))
      trace("APPLY",l(fun.args, vals))
      -- Replace current lexcial context with that of the function
      -- to be applied.  Inside a function body all names are
      -- unique, so we only need to make sure that different
      -- instantiations of the same closure use different storage.
      -- Create a new lexical frame.
      s.env = fun.env
      se.zip(function(var,val) s:def(var,val) end, fun.args, vals)
      expr = fun.body
   end


   -- Main loop
   while true do
      trace("EVAL", expr)

      s.match(
         expr,
         {
            -- ret:    return primitive value to continuation
            -- reduce: reduce without performing call or return
            -- app:    replace current context with closure
            -- push:   save context to stack

            {"(halt)", function(m)
                error({class = "expr", expr = s:ref(ret_var)})
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
                local vals = se.map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   local rv = fun(unpack(l2a(vals)))
                   if rv == nil then rv = void end
                   ret(rv)
                else
                   -- Only push when not a tail call.
                   if not is_empty(rest) then
                      push(var, rest)
                   end
                   -- Apply replaces context
                   app(fun, vals)
                end
            end},
            -- Note that 'scheme_flatten' compiler phase will collapse
            -- nested blocks like this, so we do the same here: inline
            -- it.
            {"(block . ,program)", function(m)
                rest = se.append(m.program, rest)
            end},

            {",other", function(m)
                if se.is_pair(m.other) then
                   error('bad form')
                else
                   local v = lit_or_ref(m.other)
                   assert(v)
                   ret(v)
                end
            end},
         })
   end
end


function class.eval(s,expr)
   local ok, halt_val = pcall(function() s:eval_loop(expr) end)
   assert(not ok) -- only exits via exception
   assert(halt_val and halt_val.class == 'expr')
   -- log_desc(halt_val)
   local val = halt_val.expr
   trace("HALT", val)
   return val
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
