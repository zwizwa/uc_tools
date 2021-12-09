local function push(self, v)
   assert(v)
   table.insert(self.ds, v)
   return rv
end
local function interpret(self, w)
   local f = self[w]
   if f then
      -- execute dictionary method
      assert(f)
      assert(type(f) == 'function')
      f(self)
   else
      if true then
         -- evaluate as number
         local val = tonumber(w)
         if val then
            push(self, val)
         else
            self.env:log("ERROR: " .. w .. "\n")
         end
      else
         -- evaluate as Lua expression
         -- this is often 'too smart'
         -- self.env:log("lua: " .. w .. "\n")
         local expr = loadstring("return " .. w)
         local status, val = pcall(expr)
         if status and val then
            push(self, val)
         else
            self.env:log("ERROR: " .. w .. "\n")
         end
      end
   end
end
local forth = {}
function forth.new(env)
   obj = { ds = {}, env = env, class = forth }
   setmetatable(obj, {__index = forth})
   return obj, function(w) interpret(obj, w) end
end
function forth:pop()
   local n = #self.ds
   local rv = self.ds[n]
   table.remove(self.ds, n)
   return rv
end
function forth:ps() self.env:log_desc(self.ds) end
function forth:p()  self.env:log_desc(self:pop()) end
local function op2(fun)
   return
      function(self)
         local b = self:pop()
         local a = self:pop()
         push(self, fun(a,b))
      end
end
forth['+'] = op2(function(a,b) return a+b end)
forth['-'] = op2(function(a,b) return a-b end)

return forth
