-- FIXME: Basic idea looks ok. Details are wrong.

-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives implementing 'base-ref' free variables.
-- . Simpler to implement this in Lua, using pattern matching library.

-- FIXME: NEED TO USE ENVIRONMENT TO MAP TO LOCAL STORAGE


local se       = require('lure.se')
local se_match = require('lure.se_match')

local l2a = se.list_to_array
local l = se.list

local class = {}

local void = "#<void>"

local function trace(tag, expr)
   log_se_n(expr, tag .. ":")
end

local function ifte(c,t,f)
   if c then return t else return f end
end

function class.eval_loop(s, expr, k)

   -- The environment consists of linked frames, each frame
   -- implemented by a table mapping variables to values.
   local env = {}

   -- FIXME: This is too hard to read.
   -- There are 3 operations:
   -- 1. create binding
   -- 2. reference binding value
   -- 3. set new binding value

   function def(var, val)
      assert(val ~= nil)
      trace("DEF",l(var,val))
      env[var] = val
   end
   function cell_env(var)
      local e = env
      while nil ~= e do
         if nil ~= e[var] then return e end
         e = e.parent
      end
      error("undefined variable '" .. var.unique .. "'")
   end
   function ref1(var)
      local e = cell_env(var)
      local v = e[var]
      assert(v ~= nil)
      return v
   end
   function set(var, val)
      assert(val ~= nil)
      trace("SET", l(var, val))
      local e = cell_env(var)
      e[var] = val
   end

   -- Initial continuation
   local retvar = { class = 'var' }
   local k = { var = retvar, nxt = nil, env = {} }

   -- Continuations are chained.
   -- Call with continuation.  When evaluation is done (through ret),
   -- the var will be bound, and execution resumes at block_rest.
   local function call(expr1, var, block_rest)
      assert(block_rest)
      trace("CALL",expr1,var,block_rest)
      assert(var)
      expr = expr1
      k = {expr = {'block', block_rest}, var = var, env = env, nxt = k}
   end
   local function ret(val)
      -- Restore variable and code context.
      env        = k.env
      expr       = k.expr
      -- Create the cell
      def(k.var, val)
      k          = k.nxt
   end

   -- The 'base-ref' function resolves primitives.
   local function base_ref(name)
      assert(type(name) == 'string')
      local fun = s.prim[name]
      if not fun then
         error("primitive '" .. name .. "' not defined")
      end
      trace("PRIM",name)
      return fun
   end

   -- Interpret variable reference
   local function ref(var)
      assert(var.class == 'var')
      if var.var == 'base-ref' then return base_ref end
      return ref1(var)
   end

   -- Primitive value: literal or variable referenece.
   local function lit_or_ref(thing)
      if type(thing) ~= 'table' then return thing end
      local class = thing.class
      assert(class)
      if 'var' == class then
         return ref(thing)
      elseif 'expr' == class then
         return thing.expr
      elseif 'void' == class then
         return void
      else
         error("lit_or_ref, bad class '" .. class .. "'")
      end
   end

   -- Run until the nil continuation.
   repeat
      trace("EVAL", expr)
      s.match(
         expr,
         {
            {"(block)", function(m)
                ret(void)
            end},
            {"(block (_ ,expr))", function(m)
                expr = m.expr
            end},
            {"(block (,var ,expr))", function(m)
                error("last expression in 'block' is bound: '" .. m.var .. "'")
            end},
            -- FIXME: Do primitives here.
            {"(block (_ ,expr) . ,rest)", function(m)
                call(m.expr, {class = 'var', iolist = function() return "ignore" end}, m.rest)
            end},
            {"(block (,var ,expr) . ,rest)", function(m)
                call(m.expr, m.var, m.rest)
            end},
            {"(if ,cond ,iftrue ,iffalse)", function(m)
                expr = ifte(lit_or_ref(m.cond), m.iftrue, m.iffalse)
            end},
            {"(set! ,var ,val)", function(m)
                set(m.var, lit_or_ref(m.val))
                ret(void)
            end},
            {"(lambda ,args ,body)", function(m)
                trace("LAMBDA",l(m.args, m.body))
                ret({args = m.args, body = m.body, env = env, class = 'closure'})
            end},
            {"(,fun . ,args)", function(m)
                local fun = lit_or_ref(m.fun)
                local vals = se.map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   ret(fun(unpack(l2a(vals))))
                else
                   assert(fun.args)
                   assert(fun.body)
                   assert(fun.env)
                   trace("APPLY",l(fun.args, vals))
                   -- Inside a function body all names are unique, so
                   -- we only need to make sure that different
                   -- instantiations of the same function use
                   -- different storage.  Create a new environment.
                   env = {parent = fun.env}
                   se.zip(def, fun.args, vals)
                   expr = fun.body
                end
            end},
            {",atom", function(m)
                ret(lit_or_ref(m.atom))
            end},
         })
      until (not k)
      -- return env[retvar]
      return ref(retvar)
end


function class.eval(s,expr)
   return s:eval_loop(expr)
end

function class.new()
   local s = { match = se_match.new()  }
   setmetatable(s, {__index = class})
   return s
end

return class
