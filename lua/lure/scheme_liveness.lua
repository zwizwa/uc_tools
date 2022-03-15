-- FIXME: The lambda form is not handled correctly.  A function call
-- should count as a reference to all closed-ver variables.  Not clear
-- yet how to express this.


-- Liveness analysis
--
-- Using reference count (RC) tracking it is possible to insert
-- liveness markers:
--
-- . (ref <var> 'last) to indicate last variable reference
-- . (hint 'free '(<var> ...) explicit free at start of if branch for unused variables
-- . (hint 'fanout '(<var> <rc>)) variable usage count, emitted before definition
-- . (hint 'imbalance '((<var> <rc> <rc_max> <rc_max_other>) ...) marks if branch variable use imbalance

-- Only the first two are necessary in subsequent processing.  The
-- others are likely only useful for debugging so are disabled by
-- default.

-- TODO: As a test, add functionality to scheme_eval to mark freed
-- variables to ensure that they are never referenced again.

-- The implementation uses a stateful walk & track approach that seems
-- to be reasonably effective in producing relatively simple code, but
-- it does require some ad-hoc state juggling.

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

   -- First pass collects s.rc
   s.rc = {}     -- Current "straight line" rc
   s.rc_max_if = {}  -- Branches need to save inidividual counts

   s:comp(expr)

   -- Save for toplevel.  The 'if' will set up rc_max as a parameter
   -- for subsequent branches based on what is stored in rc_max_if.
   s.rc_max = s.rc
   s.rc = {}

   -- Second pass inserts tags and hints based on RCs collected in
   -- first pass.
   local rv = s:comp(expr)

   return rv
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

local function hint(...)
   return l('_',{'hint',a2l({...})})
end
local function ins_hint(bs, ...)
   ins(bs, hint(...))
end

function class.comp_bindings(s,bindings)
   local bs = {}
   for binding in se.elements(bindings) do
      local var, vexpr = se.unpack(binding, {n=2})

      -- In second pass, reference count information will be available
      -- from first pass.  Use it for optional fanout annotation.
      if s.rc_max and var ~= '_' then
         if s.config.hint_fanout then
            ins_hint(bs, q('fanout'), q(var), s.rc_max[var])
         end
      end

      -- Init refcount table.
      if var ~= '_' then s:def(var) end

      -- Compilation will update reference counting state.
      ins(bs, l(var, s:comp(vexpr)))

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

   -- Introduce explicit (ref <var> . <annotation>) form.
   if s.rc_max and s.rc_max[var] == rc then
      return l('ref',var,q('last'))
   else
      return l('ref',var)
   end

   return var
end

-- The clause for if expressions.  Each branch has its own reference
-- count tracking.
--
-- "(if ,cond ,etrue ,efalse)"
function class.comp_if(s, expr, m)
   -- At branch entry in second pass, check the rc differences between
   -- the two branches to expose variables that can be freed in this
   -- branch.
   local function free_unused(rc, rc_max, rc_max_other, expr)
      local free = {}
      local imbalance = {}
      for var, max in pairs(rc_max) do
         local other = rc_max_other[var] or 0
         if other > max then
            -- This branch has less references to var.
            local cur = rc[var]
            ins(imbalance, l(var, cur, max, other))
            if cur == max then
               -- The case where it has no more references needs to be
               -- handled explicitly.
               ins(free, var)
            else
               -- If there are still refences to come, a 'last' tag
               -- will be inserted.
            end
         end
      end
      local bs = {}
      if s.config.hint_imbalance then
         ins(bs, hint(q('imbalance'),q(a2l(imbalance))))
      end
      ins(bs, hint(q('free'),q(a2l(free))))
      ins(bs, l('_',expr))
      return {'block', a2l(bs)}
   end

   -- Visit branches and produce final expression.
   local function comp_branch(rc, rc_max, rc_max_other, expr)
      return s:parameterize(
         {rc = rc, rc_max = rc_max},
         function()
            if rc_max then
               expr = free_unused(rc, rc_max, rc_max_other, expr)
            end
            return s:comp(s:need_block(expr))
      end)
   end

   -- Visit cond before forking, which updates s.rc
   local cond = s:comp(m.cond)

   -- Compile both branches, producing new expression.
   local function comp_fork(rc_t, rc_f, rc_t_max, rc_f_max)
      local etrue  = comp_branch(rc_t, rc_t_max, rc_f_max, m.etrue)
      local efalse = comp_branch(rc_f, rc_f_max, rc_t_max, m.efalse)
      -- Update parent ref count as max of the branches.
      for k,v in pairs(s.rc) do
         s.rc[k] = max(rc_t[k], rc_f[k])
      end
      return l('if', cond, etrue, efalse)
   end

   -- Fork the parent running refcount.
   local rc_t = tab.copy(s.rc)
   local rc_f = tab.copy(s.rc)

   -- The rc_max is recovered from the previous pass. These are
   -- attached to the 'if' syntax node via rc_max_if table.
   local rcs = s.rc_max_if[expr]
   if not rcs then
      trace("IF1", expr)
      local expr1 = comp_fork(rc_t, rc_f, nil, nil)
      -- Store branch copies for subsequent traversal.
      s.rc_max_if[expr] = {rc_t, rc_f}
      return expr1
   else
      -- Second pass uses RC structs built in first pass.
      trace("IF2", expr)
      local rc_t_max, rc_f_max = unpack(rcs)
      return comp_fork(rc_t, rc_f, rc_t_max, rc_f_max)
   end
end

-- TODO
-- 1. Capture closure
-- 2. Introduce closed over variables as 0-ref
-- 3. Find out which variables were actually used
-- 4. Use that to hoist to top level
function class.comp_lambda(s, expr, m)
   for var in se.elements(m.vars) do
      s:def(var)
   end
   return l('lambda', m.vars, s:comp(s:need_block(m.expr)))
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
             return s:comp_lambda(expr, m)
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
                return s:ref(m.other)
             end
         end}
      }
   )
end

local function new(config)
   local obj = { match = se_match.new(), indent = 0, config = config or {} }
   setmetatable(obj, { __index = class })
   return obj
end
class.new = new
return class

