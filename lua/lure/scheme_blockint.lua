-- Interpreter for the block language.
--
-- Key elements:
-- . Explicit continuations (later e.g. for call/cc)
-- . Space-safe tail recursion.
--
-- Implementation:
-- . User provides primitives through main expression's 'base-ref' parameter.
-- . Simpler to implement this in Lua, using pattern matching library.


-- Now the real question is: what would an IR be that represents the
-- iteration pattern of this interpreter?  Basically following the
-- hint that each iteration pattern defines a natural data structure
-- where each handler maps directly to a constructor.


local se       = require('lure.se')
local se_match = require('lure.se_match')
local comp     = require('lure.comp')

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

-- These operate on s.env
class.def       = comp.def
class.find_cell = comp.find_cell
class.ref       = comp.ref
class.set       = comp.set

function frame(var, expr, env)
   return {class = 'frame', var = var, expr = expr, env = env }
end

function class.eval_loop(s, expr, k)


   -- The lexical environment is implemented as a flat list.  This is
   -- slow, but very convenient for analysis.
   s.env = se.empty

   -- Top level expression is a lambda that defines the linker for all
   -- free variables present in the original source.
   s.match(
      expr,
      {{'(lambda (,base_ref) ,expr)',
        function(m)
           expr = m.expr
           assert(m.base_ref.class == 'var')
           s:def(m.base_ref,
                 function(name)
                    assert(type(name) == 'string')
                    local fun = s.prim[name]
                    if not fun then
                       error("primitive '" .. name .. "' not defined")
                    end
                    trace("PRIM",name)
                    return fun
                 end)
        end}})

   -- FIXME: Implement the continuation as a list to make it printable.
   -- Initial continuation
   local retvar = { class = 'var' }
   local k =
      {frame(
          -- Root continuation receives value in retvar and proceeds
          -- executing 'return' which will exit the loop.
          retvar,
          l('return'),
          s.env),
       se.empty}

   -- Push / pop evaluation frames.  Note that lexical environment is
   -- decoupled from this dynamic chain of stack frames.
   local function push(expr1, var, rest_block)
      assert(var)
      trace("CALL",expr1,var,block_rest)
      expr = expr1
      k = {frame(var, rest_block, s.env), k}
   end
   -- Restore execution context, storing result of subexpression evaluation.
   local function pop(val)
      -- 'def' operates on current environment, so restore that first
      local frame = se.car(k)
      s.env  = frame.env
      s:def(frame.var, val)
      expr = frame.expr
      k    = se.cdr(k)
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
                s:set(m.var, lit_or_ref(m.val))
                return void
            end},
            {"(lambda ,args ,body)", function(m)
                trace("LAMBDA",l(m.args, m.body))
                return ({args = m.args, body = m.body, env = s.env, class = 'closure'})
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
   while true do
      trace("EVAL", expr)

      -- Break out of the loop and return to caller.
      if type(expr) == 'table' and expr[1] == 'return' then
         return s:ref(retvar)
      end

      -- Primitive evaluations
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
                      s:def(m.var, val)
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
                   s.env = fun.env
                   se.zip(function(var,val) s:def(var,val) end, fun.args, vals)
                   expr = fun.body
               end},
               {",other", function(m)
                   log_se_n(expr, "BAD:")
                   error("bad form")
               end},
         })
      end
   end
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
