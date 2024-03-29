
local str  = [[
(define-syntax siso
  (syntax-rules ()
    ((_ (s i) body)
     (lambda (i)
       (rec1 (lambda (s)
         body))))))
]]

local se = require('lure.se')
local sr = require('lure.syntax_rules')
require('lure.log')

local config = {
   state = {
      gensym = function()
         return 'fake-gensym'
      end,
      env = se.empty,
   }
}
assert(config.state.gensym)
assert(config.state.env)

local function run()
   for expr in se.elements(se.read_string_multi(str)) do
      local _, name, sr_macro = se.unpack(expr, {n=3})
      sr.macro(sr_macro, config)
   end
end
return {
   run = run
}

