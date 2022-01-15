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
   local _, task, data = se.unpack(expr, {n = 3})
   local li = self:let_insert()
   task = li:maybe_insert_var(task)
   data = li:maybe_insert_var(data)
   if not li:compile_inserts(l('co',task,data)) then
      -- _0 = data; s->next=&&l1; s=task; goto s->next; l1: rv = _0;
      local label = self:gensym("l")
      local s = self.config.state_name
      local nxt = {s,"->next"}
      local c_task = self:atom_to_c_expr(task)
      local c_data = self:atom_to_c_expr(data)
      -- Store data. Coroutine value passing re-uses the registers
      -- used for function calls.
      self:w(self:tab(), self:arg(0), " = ", c_data, ";\n")
      -- Set current task's resume point.
      self:w(self:tab(), nxt, " = &&", label, "; ")
      -- Switch task pointer and jump to task's resume point.
      self:w(s, " = ", c_task, "; goto *", nxt, "; ")
      -- Local variables are lost after goto.
      self:local_lost()
      -- Other task will jump back to this resume point.
      self:w(label, ":\n")
      self:w(self:binding(self:arg(0)))
      self:mark_bound(self.var)
   end
end

form['yield'] = function(self, expr)
   local label = self:gensym("l")
   local s = self.config.state_name
   local nxt = {s,"->next"}
   -- Set current task's resume point.
   self:w(self:tab(), nxt, " = &&", label, "; ")
   self:w("return; ");
   -- Local variables are lost after return.
   self:local_lost()
   -- Other task will jump back to this resume point.
   self:w(label, ":\n");
end


return form
