-- Utility code shared between different backends.  These are written
-- as mixins, which seems to be the most convenient composition
-- mechanism.

local se = require('lure.se')

local mod = {}

-- Unpack the top level lambda as a sequence of lib_ref bindings + a
-- labels form.  This is the preferred form for languages that produce
-- output modules built from a set of mutually recursive top level
-- functions.
function mod.match_module_form(s, expr, lib_def, compile_labels)
   s.match(
      expr,
      {{"(lambda (,lib_ref) (block . ,bindings))", function(m)
           -- Assumptions:
           -- . Last binding is a labels form
           -- . All other bindings are lib_ref

           for binding, rest in se.elements(m.bindings) do
              if not se.is_empty(rest) then
                 -- Collect binding
                 s.match(
                    binding,
                    {{"(,var (app ,lib_ref ,sym))", function(b)
                         assert(m.lib_ref == b.lib_ref)
                         lib_def(b.var, b.sym)
                     end}})
              else
                 -- Compile top-level functions
                 s.match(
                    binding,
                    {
                       {"(_ (labels ,bindings ,inner))", function(l)
                           compile_labels(l.bindings, l.inner)
                       end},
                       {",other", function(b)
                           _trace("BAD",b.other)
                           error("bad_binding")
                       end}
                    })
              end
           end
       end}})
end

return mod
