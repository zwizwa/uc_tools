
-- DFL - Data Flow Language in final tagless style
--
-- Context is pure event-driven data flow systen design, with
-- simulation in lua + compilation to C with static metadata.

local dfl = {
   -- semantics objects
   eval = {},
   comp = {},
}

-- A choice is made to stick to lua semantics and make all function
-- variadic.  Going against that will introduce a lot of red tape.
-- The only downside seems to be that, when compiling a function, the
-- arity needs to be specified.
function dfl.compile(syntax, arity)
   -- Initialize compilation context
   local comp = {
      next_node = 1,
      nodes = {},
      inputs = {},
      arity = arity,
   }
   setmetatable(comp, { __index = dfl.comp })
   -- Create input nodes.
   for i=1,arity do
      table.insert(comp.inputs, comp:node())
   end
   -- Invoke higher order abstract syntax object to produce
   -- intermediate nodes as a side effect, and return a list of output
   -- nodes.
   function pack(...) return {...} end
   comp.output = pack(syntax(comp, unpack(comp.inputs)))
   return comp
end

function dfl.comp:node(opcode, operands)
   if not opcode then opcode = "input" end
   if not operands then operands = {} end
   local node = { opcode = opcode, operands = operands }
   local ref = self.next_node
   self.nodes[ref] = node
   self.next_node = ref + 1
   return ref
end

-- It's convenient to define compile and eval semantics at the same
-- time, by reusing function names for compile time semantics.
local f = {}
function f.add(a,b) return a+b end
function f.sub(a,b) return a-b end
function f.mul(a,b) return a*b end
function f.div(a,b) return a/b end
-- Create semantics functions for eval and comp semantics from this table.
for name, func in pairs(f) do
   dfl.eval[name] = function(self, ...)
      -- Evaluation applies the function to the arguments directly.
      return func(...)
   end
   dfl.comp[name] = function(self, ...)
      -- Compilation creates a new node that represents the function's
      -- target language code.
      return self:node(name, {...})
   end
end

-- C code gen assumes a context object.  This might be useful later
-- and also avoids issues with comma generation here.
function dfl.print(syntax, arity, fun_name)
   local dag = dfl.compile(syntax, arity)
   local out = {}
   local t = "int"
   local tab = "    "
   local ctx = "_"
   local function w(str) table.insert(out, str) end
   w(t); w(" "); w(fun_name); w("(void*"); w(ctx)
   for i,input in ipairs(dag.inputs) do
      w(", "); w(t); w(" "); w("r"); w(input);
   end
   w(") {\n")
   for i, n in ipairs(dag.nodes) do
      if n.opcode ~= "input" then
         w(tab); w(t); w(" r"); w(i); w(" = "); w(n.opcode);
         w("("); w(ctx);
         for j,operand in ipairs(n.operands) do
            w(", r"); w(operand);
         end
         w(")\n")
      end
   end
   w(tab); w("return r"); w(dag.output[1]); w(";\n")
   w("}\n")
   return table.concat(out)
end



return dfl


