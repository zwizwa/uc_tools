-- Liveness analysis
--
-- Constructs a reference count for each variable, which provides
-- enough information to produce hints for rc init and markers for
-- last use.
--
-- FIXME: This is not correct for conditionals.

local se        = require('lure.se')
local se_match  = require('lure.se_match')
local iolist    = require('lure.iolist')
local lure_comp = require('lure.comp')
local tab       = require('lure.tab')
local l = se.list
local a2l = se.array_to_list
local ins = table.insert
local pop = table.remove

local scheme_frontend = require('lure.scheme_frontend')

local class = {}

-- Cherry-pick some methods (micro-mixin?)
class.parameterize = lure_comp.parameterize
class.indented     = lure_comp.indented
class.tab          = lure_comp.tab


local function _trace(tag, expr)
   log('\n') ; log_se_n(expr, tag .. ": ")
end
local function trace(tag, expr)
   -- _trace(tag, expr)
end

function q(expr)
   return scheme_frontend.quote(expr)
end

function class.compile(s,expr)
   s.free = {}
   s.rc_if = {}

   -- First pass collects s.rc
   s.rc = {}
   s:comp(expr)
   s.rc_max = s.rc

   -- Second pass inserts hints for rc init and variable free.
   s.rc = {}
   local rv = s:comp(expr)

   return rv
end

function ifte(a,b,c)
   if a then return b else return c end
end
function max(a,b)
   if a>b then return a else return b end
end

-- There needs to be room to insert the hints, so "naked" expressions
-- are converted to blocks.
function class.need_block(s,expr)
   return s.match(
      expr,
      {
         {"(block . ,_)", function(m)
             return expr
         end},
         {",other", function(m)
             return l('block',l('_',m.other))
         end}
      })
end

function class.comp_bindings(s,bindings)
   local bs = {}
   local function hint(...)
      ins(bs, l('_',se.append(l('hint'),a2l({...}))))
   end
   for binding in se.elements(bindings) do
      local var, vexpr = se.unpack(binding, {n=2})
      -- In second pass, reference count information will be available
      -- from first pass.
      if s.rc_max and var ~= '_' then
         -- FIXME: This is maybe not that useful?  The 'last' hint is enough.
         hint(q('rc'),q(var),s.rc_max[var])
      end

      -- Rebuild refcount information.  In pass 1 this builds the RC
      -- table.  In pass 2 this does the same to find the point where
      -- RC=RC_MAX.
      if var ~= '_' then s:def(var) end

      -- Compilation will update reference counting state.
      local vexpr1 = s:comp(vexpr)

      -- Insert free hints.  These need to go _before_ the binding, to
      -- not mess up the last expression == return value property.
      -- The hint is called 'last' to indicate that the next binding
      -- has the last reference to this variable.
      if #s.free ~= 0 then
         hint(q('last'),q(a2l(s.free)))
         s.free = {}
      end

      ins(bs, l(var, vexpr1))

   end
   return a2l(bs)
end

function class.def(s,var)
   if s.rc[var] ~= nil then
      _trace("VAR", var)
      error("bad_def")
   end
   s.rc[var] = 0
end

function class.ref(s,var)
   local rc = s.rc[var]
   if rc == nil then
      _trace("VAR",var)
      error("bad_ref")
   end
   rc = rc + 1
   s.rc[var] = rc
   if s.rc_max and s.rc_max[var] == rc then
      assert(s.free)
      ins(s.free, var)
   end
end

-- The clause for if expressions.  Each branch has its own reference
-- count tracking.
--
-- "(if ,cond ,etrue ,efalse)"
function class.comp_if(s, expr, m)

   -- Visit cond before forking
   local cond = s:comp(m.cond)

   -- The two branches have separate refcounts.  These are attached to
   -- the 'if' node in the syntax tree through the rc_if table.
   local rc_t, rc_f = s.rc_if[expr]

   -- Visit branches and produce final expression.
   local function fork()
      local function branch(rc, expr)
         return s:parameterize(
            {rc = rc},
            function()
               -- FIXME: With rc struct switched, it is possible that
               -- some variables have reached their max refcount in
               -- this branch, so iterate over all to insert 'free'
               -- hints.
               return s:comp(s:need_block(expr))
         end)
      end
      local etrue  = branch(rc_t, m.etrue)
      local efalse = branch(rc_f, m.efalse)
      return l('if', cond, etrue, efalse)
   end

   -- Pass 1 and 2 are different.
   if rc_t and rc_f then
      -- Second pass uses RC structs built in first pass.
      _trace("IF2", expr)
      return fork()
   else
      _trace("IF1", expr)
      -- First pass: snapshot parent rc for t/f branches
      rc_t = tab.copy(s.rc)
      rc_f = tab.copy(s.rc)
      -- Store branch copies for subsequent traversal.
      s.rc_if[expr] = {rc_t, rc_f}
      local expr1 = fork()
      -- Increment ref count for existing variables.
      for k,v in pairs(s.rc) do
         s.rc[k] = max(rc_t[k], rc_f[k])
      end
      return expr1
   end
end

function class.comp(s,expr)
   return s.match(
      expr,
      {
         {"(block . ,bindings)", function(m)
             trace("BLOCK", expr)
             return {'block', s:comp_bindings(m.bindings)}
         end},
         {"(lambda ,vars ,expr)", function(m)
             trace("LAMBDA", expr)
             for var in se.elements(m.vars) do
                s:def(var)
             end
             return l('lambda', m.vars, s:comp(s:need_block(m.expr)))
         end},
         {"(if ,cond ,etrue ,efalse)", function(m)
             return s:comp_if(expr, m)
         end},
         {"(app ,fun . ,args)", function(m)
             trace("APP", expr)
             local fun  = s:comp(m.fun)
             local args = se.map(function(a) return s:comp(a) end, m.args)
             return {'app', {fun, args}}
         end},
         {"(hint . ,_)", function(m)
             return expr
         end},
         {",other", function(m)
             if (se.expr_type(m.other) ~= 'var') then
                return expr
             else
                s:ref(m.other)
                return expr
             end
         end}
      }
   )
end

local function new()
   local obj = { match = se_match.new(), indent = 0 }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

