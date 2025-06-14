-- Grab bag of tools for ad-hoc HOAS DSL encoding in Lua.
-- Part of uc_tools/lua -- https://opensource.org/licenses/MIT
local m = {}

-- Some design decisions:

-- 1. Don't use numbers to represent variables.  Lua has interned
--    strings so just use those.  If later a transition to numbers is
--    necessary it can be done as a separate translation phase.
--
-- 2. Represent composite nodes by string concatenation, and keep
--    track of a composite name -> vector of component names.


-- Create a symbol table builder, exposed as a table.
local function def_ref(symtab)
   return function(obj, str)
      if symtab.var[str] == nil then
         logf("decl %s:%s\n", symtab.tag, str)
         symtab.var[str] = true
      end
      return str
   end
end
local function symtab_comp(symtab)
   local obj = {}
   setmetatable(obj, {__index = def_ref(symtab)})
   return obj
end
m.symtab_comp = symtab_comp

local function symtab_init(tag)
   return {
      var = {},  -- set of variables
      tag = tag, -- which namespace
   }
end
m.symtab_init = symtab_init


-- Create product (composite) nodes.
function product(compstate, tab)
   local strs = {}
   for _, key in ipairs(compstate.state_keys) do
      table.insert(strs, string.format("%s=%s", key, tab[key]))
   end
   local prod_key = table.concat(strs, '\n')
   compstate.state[prod_key] = tab
   return prod_key
end
m.product = product


-- Convert finite state machine to graphviz.
-- See http://sfriederichs.github.io/how-to/graphviz/2017/12/07/State-Diagrams.html

local function fsm_to_dot(fmt, compstate)
   -- log_desc({fsm_dot_compstate=compstate})
   local state  = compstate.state
   local trans  = compstate.trans

   fmt("digraph b2b_fsm {\n")
   fmt("node [shape=circle];\n")

   -- Create the nodes, keep track of the numbers.
   local node_n = {}
   local n=1
   for s in pairs(state) do
      fmt('node%s[label="%s"];\n', n, s)
      node_n[s] = n
      n=n+1
   end
   for _, t in pairs(trans) do
      fmt('node%s->node%s[label="%s"];\n', node_n[t.from], node_n[t.to], t.event)
   end
   fmt("}\n")
end
m.fsm_to_dot = fsm_to_dot



return m
