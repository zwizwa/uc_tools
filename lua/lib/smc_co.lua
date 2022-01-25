-- Co-routine form for smc.lua
--
-- Couroutines miss synchronization, but they are a simpler mechanism
-- that can be used in its own right, and to test the compiler.  So
-- this intends to implement:

-- 1. Coutines

-- 2. CSP (later)

-- EDIT: Below needs revision.

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
   assert(v_chan)
   -- log_desc({v_chan = v_chan})
   assert(v_chan.val)

   -- For now just transfer between 2 coroutines.
   local t = self.current_task
   self:w("/*sch:",t,"*/ ")
   local other_task = 1 - t
   local other_nxt = self:next(other_task)
   return other_nxt
end


local function co(self, form, chan, data)
   local li = self:let_insert()
   chan = li:maybe_insert_var(chan)
   if data then
      data = li:maybe_insert_var(data)
   end
   if not li:compile_inserts(l(form,chan,data)) then

      self:w(self:tab())

      if (data) then
         -- Store data to be transferred. Coroutine value passing re-uses
         -- the registers used for function tail calls.
         local c_data = self:atom_to_c_expr(data)
         self:w(self:arg(0), " = ", c_data, "; ")
      end

      -- Set current task's resume point.
      local label = self:gensym("l")
      local self_nxt = self:next()
      self:w(self_nxt, " = &&", label, "; ")

      -- Perform scheduling
      -- The task varariable is ephemeral.  Scheduling is performed at
      -- compile time as much as possible.
      local other_nxt = schedule(self, chan)

      self:w("goto *", other_nxt, "; ")
      -- Other task will jump back to this resume point.
      -- Semicolon after label acts as statement.
      self:w(label, ":;\n")
      -- C local variables are lost when we jump back into this code.
      self:local_lost()

      self:w(self:binding(self:arg(0)))
      self:mark_bound(self.var)
   end
end

form['co'] = function(self, expr)
   local _, chan, data = se.unpack(expr, {n = 3})
   co(self, 'co', chan, data)
end
form['write'] = function(self, expr)
   local _, chan, data = se.unpack(expr, {n = 3})
   co(self, 'write', chan, data)
end
form['read'] = function(self, expr)
   local _, chan = se.unpack(expr, {n = 2})
   co(self, 'read', chan, nil)
end



form['yield'] = function(self, expr)
   local _, data = se.unpack(expr, {n = 2})
   local li = self:let_insert()
   data = li:maybe_insert_var(data)
   if not li:compile_inserts(l('yield',data)) then
      local label = self:gensym("l")
      local s = self.config.state_name
      -- Set network resume point and exit network.  Note that we do
      -- not need to save the task's resume point, since C function
      -- entry will resume here via s->next.
      self:w(self:tab(),
             self.config.state_name,"->next",
             " = &&", label, "; ")
      local c_data = self:atom_to_c_expr(data)
      self:w("return ", c_data, "; ");
      -- Local variables are lost after return.
      self:local_lost()
      -- Other task will jump back to this resume point.
      -- Semicolon after label acts as statement.
      self:w(label, ":;\n");
   end
end


return form
