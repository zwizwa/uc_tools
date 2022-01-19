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

require('lib.log')

local function schedule(self, chan)
   -- FIXME: Later, get scheduler info from compile-time schannel variable
   local v_chan = self:ref(chan)
   -- log_desc({v_chan = v_chan})
   assert(v_chan.val)

   -- For now just transfer between 2 coroutines.
   local t = self.current_task
   self:w("/*sch:",t,"*/ ")
   local other_task = 1 - t
   local other_nxt = self:next(other_task)
   return other_nxt
end


form['co'] = function(self, expr)
   local _, chan, data = se.unpack(expr, {n = 3})
   local li = self:let_insert()
   chan = li:maybe_insert_var(chan)
   data = li:maybe_insert_var(data)
   if not li:compile_inserts(l('co',chan,data)) then
      -- _0 = data; s->next=&&l1; s=task; goto s->next; l1: rv = _0;
      local label = self:gensym("l")
      local s = self.config.state_name
      local nxt = self:next()
      local c_data = self:atom_to_c_expr(data)

      -- Store data to be transferred. Coroutine value passing re-uses
      -- the registers used for function tail calls.
      self:w(self:tab(), self:arg(0), " = ", c_data, "; ")
      -- Set current task's resume point.
      self:w(nxt, " = &&", label, "; ")
      -- Perform scheduling
      -- The task varariable is ephemeral.  Scheduling is performed at
      -- compile time as much as possible.
      local other_nxt = schedule(self, chan)

      self:w("goto *", other_nxt, "; ")
      -- Other task will jump back to this resume point.
      self:w(label, ":\n")
      -- C local variables are lost when we jump back into this code.
      self:local_lost()
      self:w(self:binding(self:arg(0)))
      self:mark_bound(self.var)
   end
end

form['yield'] = function(self, expr)
   local label = self:gensym("l")
   local s = self.config.state_name
   local nxt = self:next()
   -- Set current task's resume point.
   self:w(self:tab(), nxt, " = &&", label, "; ")
   self:w("return; ");
   -- Local variables are lost after return.
   self:local_lost()
   -- Other task will jump back to this resume point.
   self:w(label, ":\n");
end


return form
