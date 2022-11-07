-- FIXME: Just moving it out of the way for now.
-- Some sybols unbound.

local function w_scheme(prog)

   -- FIXME: abstract with-separator

   local function w_expr(w, code)
      if is_signal(code) then
         w('(',code.op," ",code.arg)
         if code.index then
            w(" ")
            w_expr(w, code.index)
         end
         w(')')
      elseif se.is_pair(code) then
         w('(')
         for c, nxt in se.elements(code) do
            w_expr(w, c)
            if (se.is_pair(nxt)) then w(' ') end
         end
         w(')')
      else
         w(code)
      end
   end
   local tab = ''

   local function w_seq(w, code)
      for c in se.elements(code) do
         if (se.car(c) == 'for-index') then
            local _, index, n, sub_code = se.unpack(c, {n=4})
            w(tab, '(for-index ', index, " ", n, "\n")
            local saved_tab = tab ; tab = tab .. '  '
            w_seq(w, sub_code)
            tab = saved_tab
            w(tab,')\n')
         elseif (se.car(c) == 'alloc') then
            -- Technically this is a 2nd pass
            local _, var, typ = se.unpack(c, {n=3})
            if prog.is_out[var] then
               w(';; out: ')
            end
            w_expr(w,c)
            w('\n')
         else
            w(tab)
            w_expr(w, c)
            w('\n')
         end
      end
   end


   local function w_prog(w, prog)
      w("\n")
      w("types:\n")
      for k,v in pairs(prog.types) do
         w(k,": ")
         w_expr(w, v)
         w("\n")
      end
      w("state:\n")
      for s in se.elements(prog.state) do
         w_expr(w, s)
         w("\n")
      end
      w("args: ")
      w_expr(w, prog.args)
      w("\n")
      w("code:\n")
      w_seq(w, prog.code)
      w("return:\n")
      for _,c in ipairs(prog.out) do
         w_expr(w, c)
         w('\n')
      end
   end

   w_prog(w, prog)
end



