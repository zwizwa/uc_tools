-- Co-routine form for smc.lua
--
-- Idea is to split up CSP implementation by first implementing a
-- simple form of co-routines as an optimization for CSP task
-- scheduling that doesn't need to touch any data structures
-- (e.g. task with single blocking channel), and to implement the more
-- complex part of the CSP scheduler as a coroutine directly in
-- Scheme code.

-- What should a coroutine call do?  Pass a single value to another
-- task.  If multiple values are needed, indirection can be
-- implemented later.  For now we are still focused on the minimal
-- base case.

-- Should the coroutine reference be a first class value?  That's
-- probably simplest.

local se = require('lib.se')
local l = se.list

local form = {}

form['co'] = function(self, expr)
   local _, other, data = se.unpack(expr, {n = 3})
   local li = self:let_insert()
   other = li:maybe_insert_var(other)
   data  = li:maybe_insert_var(data)
   if not li:compile_inserts(l('co',other,data)) then
      -- _0 = data; s->next=&&l1; s=other; goto s->next; l1: rv = _0;
      local label = self:gensym("l")
      local s = self.config.state_name
      local nxt = {s,"->next"}
      self:w(self:tab(), self:arg(0), " = ", self:atom_to_c_expr(data), ";\n")
      self:w(self:tab(), nxt, " = &&", label, ";\n")
      self:w(self:tab(), s, " = ", self:atom_to_c_expr(other), "; goto *", nxt, ";\n")
      self:local_lost()
      self:w(self:tab(), label, ": ", self:var_def(self.var), self:arg(0), ";\n")
      self:mark_bound(self.var)
   end
end

return form
