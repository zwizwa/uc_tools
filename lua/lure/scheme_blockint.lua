-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives implementing 'base-ref' free variables.
-- . Simpler to implement this in Lua, using pattern matching library.


local se       = require('lure.se')
local se_match = require('lure.se_match')

local l2a = se.list_to_array
local l = se.list

local class = {}

local void = "#<void>"

local function trace(tag, expr)
   -- log_se_n(expr, tag .. ":")
end

local function ifte(c,t,f)
   if c then return t else return f end
end

function class.eval_loop(s, expr, k)

   -- The environment consists of linked frames, each frame
   -- implemented by a table mapping variables to values.
   local env = {}

   -- Create binding, always in current environment.  Called on 'ret'
   -- and for function arguments on frame entry.
   function def(var, val)
      assert(val ~= nil)
      trace("DEF",l(var,val))
      env[var] = val
   end
   -- Reference ans assigment operate on the chained environment.  One
   -- table per function activation, linked by 'parent' member.
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
   local k = { var = retvar, parent = nil, env = {} }

   -- Push / pop evaluation frames.  Note that lexical environment is
   -- decoupled from this dynamic chain of stack frames.
   local function push(expr1, var, rest_block)
      assert(var)
      trace("CALL",expr1,var,block_rest)
      expr = expr1
      k = {expr = rest_block, var = var, env = env, parent = k}
   end
   -- Restore execution context, storing result of subexpression evaluation.
   local function pop(val)
      -- 'def' operates on current environment, so restore that first
      env  = k.env
      def(k.var, val)
      expr = k.expr
      k    = k.parent
   end

   -- The lexical environment is extended with one magic variable
   -- 'base-ref', which is used to obtain references to primitives.
   local function base_ref(name)
      assert(type(name) == 'string')
      local fun = s.prim[name]
      if not fun then
         error("primitive '" .. name .. "' not defined")
      end
      trace("PRIM",name)
      return fun
   end
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

   -- Primitive evaluations that terminate immediately.
   -- This returns nil if not primitive.
   local function prim_eval(expr)
      return s.match(
         expr,
         {
            {"(block)", function(m)
                return void
            end},
            {"(set! ,var ,val)", function(m)
                set(m.var, lit_or_ref(m.val))
                return void
            end},
            {"(lambda ,args ,body)", function(m)
                trace("LAMBDA",l(m.args, m.body))
                return ({args = m.args, body = m.body, env = env, class = 'closure'})
            end},
            {"(app ,fun . ,args)", function(m)
                local fun = lit_or_ref(m.fun)
                local vals = se.map(lit_or_ref, m.args)
                if 'function' == type(fun) then
                   return fun(unpack(l2a(vals)))
                else
                   -- Closure evaluation is not primitive.
                   return nil
                end
            end},
            {",other", function(m)
                if se.is_pair(m.other) then
                   return nil
                else
                   local v = lit_or_ref(m.other)
                   assert(v)
                   return v
                end
            end},
         })
   end

   -- Run until the nil continuation.
   repeat
      trace("EVAL", expr)

      local val = prim_eval(expr)
      if val ~= nil then
         trace("PRIMVAL",val)
         -- Evaluation in this context has ended.  Return to previous
         -- context, passing the return value.
         pop(val)
      else
         s.match(
            expr,
            {
               -- Inplace reductions
               {"(if ,cond ,iftrue ,iffalse)", function(m)
                   expr = ifte(lit_or_ref(m.cond), m.iftrue, m.iffalse)
               end},
               {"(block (_ ,expr))", function(m)
                   expr = m.expr
               end},
               {"(block (,var ,expr))", function(m)
                   error("last expression in 'block' is bound: '" .. m.var .. "'")
               end},

               -- Reduce sequence
               {"(block (,var ,expr) . ,rest)", function(m)
                   if m.var == '_' then
                      m.var = {class = 'var', iolist = "_"}
                   end
                   local rest_block = {'block', m.rest}
                   local val = prim_eval(m.expr)
                   if val then
                      -- As an optimization, we can perform primitive
                      -- evaluation without a push/pop sequence.
                      trace("PRIMBIND", val)
                      def(m.var, val)
                      expr = rest_block
                   else
                      -- For all the rest we switch evaluation context
                      -- to focus on the subexperssion.
                      push(m.expr,     -- subexpression to evaluate
                           m.var,      -- return value goes here
                           rest_block) -- evaluation resumes here
                   end
               end},

               -- Enter closure
               {"(app ,fun . ,args)", function(m)
                   local fun = lit_or_ref(m.fun)
                   local vals = se.map(lit_or_ref, m.args)
                   -- Primitives handled elsewhere.
                   assert('function' ~= type(fun))
                   trace("APPLY",l(fun.args, vals))
                   -- Replace current lexcial context with that of the
                   -- function to be applied.  Inside a function body
                   -- all names are unique, so we only need to make
                   -- sure that different instantiations of the same
                   -- closure use different storage.  Create a new
                   -- lexical frame.
                   env = {parent = fun.env}
                   se.zip(def, fun.args, vals)
                   expr = fun.body
               end},
               {",other", function(m)
                   log_se_n(expr, "BAD:")
                   error("bad form")
               end},
         })
      end
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
