local se = require('lure.se')
require('lure.log')
local m = {}

-- Split the list of expressions into syntax definitions (with
-- 'define-syntax' mapped to 'define') and other forms.

function m.split(exprs)
   local macro_exprs = se.empty
   local rest_exprs = se.empty
   for expr in se.elements(exprs) do
      if se.is_expr(expr, 'define-syntax') then
         local new_expr = se.cons('define', se.cdr(expr))
         macro_exprs = se.cons(new_expr, macro_exprs)
      else
         rest_exprs = se.cons(expr, rest_exprs)
      end
   end
   return
      se.reverse(macro_exprs),
      se.reverse(rest_exprs)
end

return m
