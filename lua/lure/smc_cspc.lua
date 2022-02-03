-- Mixin implementing 'read','write','select' forms for uc_tools/csp.c

-- The main interface to the shared compiler is :local_lost(), which
-- marks a yield point, conceptually marking all variables as lost.
-- Variables that are marked as lost AND are later referenced, can not
-- be implemented as C local variables.

local se = require('lure.se')
local l = se.list

local form = {}

-- Generate csp.h CSP_EVT and CSP_SEL macro invocations + dispatch on
-- ->selected, starting from a multi-clause Scheme form:
--
-- (select
--   ((read  0 <var>) (... ref <var> ...))
--   ((write 1 <val>) (...))
--
-- This call blocks execution and resumes exactly one of the clauses.


-- FIXME
form['select'] = function(self, expr)
   local _, clauses_expr = se.unpack(expr, {n = 1, tail = true})
   -- Collect read and write clauses separately.  They need to be
   -- sorted before invoking CSP_SEL.
   local clauses = { read = {}, write = {} }
   -- Also rebuild the expression in case let insertion is necessary
   local clauses_expr1 = se.empty
   -- Keep track of bindings to perform ANF transformation if necessary
   local li = self:let_insert()
   for clause_expr in se.elements(clauses_expr) do
      self:w(self:se_comment_i_n(clause_expr))
      local head_expr, handle_expr = se.unpack(clause_expr, {n = 2})
      local kind, chan, data_expr  = se.unpack(head_expr,   {n = 3})
      if kind == 'write' then
         -- Possibly needs let insertion.
         data_expr = li:maybe_insert_var(data_expr)
      else
         assert(kind == 'read')
      end
      assert(type(data_expr) == 'string')
      table.insert(clauses[kind],
                   {chan = chan,
                    var  = data_expr,
                    expr = handle_expr})
      clauses_expr1 = {l(l(kind, chan, data_expr), handle_expr),
                       clauses_expr1}
   end

   -- Insert let if there were any non-variable forms.
   if li:compile_inserts({'select', se.reverse(clauses_expr1)}) then
      return
   end

   local s = self.config.state_name
   local t = {"&(",s,"->task)"}

   local function w(...)
      self:w(self:tab(),{...})
   end

   -- The C case statement needs separate variable definition and
   -- assignment.  For this the variable is marked as assign_later, such
   -- that subsequent binding operations ignore that the variable has
   -- already been bound and emit an assignment insted of a
   -- definition.
   self:w(self:tab(), self:var_def_assign_later(self.var), "{\n")
   self:save_context(
      {'indent','env','stack_ptr'},
      function()
         self:inc('indent')

         -- Note: the CSP scheduler is used in zero-copy mode by
         -- setting rcv buffer pointer to NULL.  Further we only use
         -- the msg_buf in unboxed mode (unitptr_t machine word
         -- .msg.buf.w).
         local function stmt(...)
            self:w(self:tab(),self:statement(unpack({...})))
         end

         local n_w = #(clauses.write)
         local n_r = #(clauses.read)
         self:track_max_indexed('evt_size', self.current_task, n_w + n_r)

         for i,c in ipairs(clauses.write) do
            local cvar = self:atom_to_c_expr(c.var)
            stmt("CSP_EVT_BUF",t,i-1,c.chan,cvar,0)
         end
         for i,c in ipairs(clauses.read) do
            stmt("CSP_EVT_BUF",t,n_w+i-1,c.chan,"NULL",0)
         end

         stmt("CSP_SEL",t,s,n_w,n_r)
         self:local_lost()

         local function w_case(evt,c,bind_var)
            w("case ", evt, ": {\n")
            self:save_context(
               {'indent','env','stack_ptr'},
               function()
                  self:inc('indent')
                  if bind_var then
                     local v = self:new_var(bind_var)
                     self:w(self:tab(),self:var_def(v),
                            s,"->evt[",evt,"].msg.w;\n")
                     self:mark_bound(v)
                     self:push_var(v)
                  end
                  self:compile(c.expr)
                  w("} break;\n");
               end)
         end

         w("switch((",t,")->selected) {\n")
         for i,c in ipairs(clauses.write) do w_case(i-1,c) end
         for i,c in ipairs(clauses.read)  do w_case(n_w+i-1,c,c.var) end
         w("}\n")
   end)
   w("};\n")


end

form['read'] = function(self, expr)
   local _, chan = se.unpack(expr, {n = 2})
   self:local_lost()
   local s = self.config.state_name
   local t = {"&(", s, "->task)"}
   self:w(self:tab(),
          self:var_def(self.var),
          self:statement("CSP_RCV_W", t, s, chan))
   self:track_max('evt_size', 1)
   self:mark_bound(self.var)
end

form['write'] = function(self, expr)
   local _, chan, data_expr = se.unpack(expr, {n = 3})

   local li = self:let_insert()
   data_expr = li:maybe_insert_var(data_expr)
   if li:compile_inserts(l('write', chan, data_expr)) then
      return
   end

   -- Perform the reference _before_ marking the context as lost.  Our
   -- reference here is used to initialize the evt msg before
   -- executing return.
   local cvar = self:atom_to_c_expr(data_expr)

   self:local_lost()
   local s = self.config.state_name
   local t = {"&(", s, "->task)"}
   self:w(self:tab(),self:statement("CSP_SND_W", t, s, chan, cvar))
   self:track_max('evt_size', 1)
end


return form


