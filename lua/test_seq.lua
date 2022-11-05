#!./lua.sh


-- Rework: this is a proof of concept interpretation model + compiler.
-- It is best to switch from this approach to Lure intermediate
-- language.

-- Split into:
-- . lazy list lib (done)
-- . compile 'unfold into array' (done)
-- . compile from hoas to Scheme syntax (+- done)
-- . compile Lure IR extended with vector loops to CPS (return slot pointer only)


-- SYNTAX

-- The first argument contains the library of signal operations.
-- The signals support overloading for arithmetic operations.
-- Library functions will auto-lift numbers to signals.

require 'lure.log'
local se = require 'lure.se'

-- FIXME: curry the context argument.
-- FIXME: in should be a single argument

local function prog1(c, i)
   local function counter_sm(s) return s+1, s end
   local counter = c.close(0, counter_sm)
   return counter + i
end

local function prog2(c)
   function update(s) return c.add1(s), s end
   return c.close(0, update)
end

local function prog3(c, a)
   return c.add1(a)
end

local function prog4(c, a, b)
   return c.add(a, b)
end

local function prog5(c, a)
   return a + 1
end

local function prog6(c, a)
   return c.vec(3, function(i) return i + 1 end)
end

local function prog7(c, a)
   local a = c.vec(
      -- Vector size needs to be known at compile time (for now).
      13,
      function(
            -- Index into array
            i,
            -- Dereference for elements already computed.  ref(j) is
            -- valid for j<i.  If this is not used, the computation is
            -- not causally linked so it can be performed in parallel.
            ref)
         -- The return value is what goes into slot i
         return i + i + 1
      end)
   -- Later, implement auto-lifting of simple constructs.
   return a + 1
end





-- SEMANTICS: SIGNALS AS LAZY LISTS

-- Use lazy lists to represent signals.
local ll_metatable = {}
local ll = require('lure.lazylist')(ll_metatable)

local signal_c = {
   add1  = ll.lift(1, function(a)   return a+1 end),
   add   = ll.lift(2, function(a,b) return a+b end),
   close = ll.close
}

-- Patch the metatable for operator support.
ll_metatable.__add = signal_c.add

local f1 = prog1(signal_c, ll.pure(100))
local f2 = prog2(signal_c)

-- log_desc({
--       one = c.one,
--       add1_one = c.add1(c.one),
--       one_tail = c.one.tail(),
--       add1_one_tail = c.add1(c.one).tail(),
--       f2 = f2,
--       f2_tail = f2.tail(),
--       f2_tail_tail = f2.tail().tail(),
--       f1 = f1,
--       f1_tail = f1.tail(),
-- })

log_desc({
      f1 = ll.take(3,f1),
      f2 = ll.take(3,f2)
})

local function counter(n)
   return function()
      local val = n
      n = n + 1
      return val
   end
end


-- SEMANTICS: COMPILER

-- Erlang's iolist
local function w_thing(thing)
   if type(thing) == 'table' then
      for _, subthing in ipairs(thing) do
         w_thing(subthing)
      end
   else
      io.stdout:write(thing .. "")
   end
end
local function w(...) w_thing({...}) end

local function compile(prog, nb_input)
   -- State
   local code = {}
   local new_var_number = counter(1)
   local types = {}
   local new_state = counter(0)
   local init_state = {}

   local function var_name(var_nb)
      -- Variables are not ordered, so just represent them as strings.
      return 'v' .. var_nb
   end
   local function new_var(typ)
      local v = var_name(new_var_number())
      -- type will be patched later by access pattern
      types[v] = typ or "T"
      return v
   end

   local signal_mt = {} -- patched later for Lua operator overloading
   local signal_type = {'signal'}  -- table instance is unique tag
   local function signal(...)
      local rep = {...}
      rep.type = signal_type
      setmetatable(rep, signal_mt)
      return rep
   end
   local function is_signal(thing)
      return type(thing) == 'table' and thing.type == signal_type
   end
   local function project(thing)
      if is_signal(thing) then return thing end
      return signal('lit', thing)
   end
   local function ref(var)
      return signal('ref', var)
   end
   local function app(op, args_)
      local var = new_var()
      local args = {}
      for i,a in ipairs(args_) do args[i] = project(a, lit) end
      table.insert(code, {'let', var, {op, args}})
      return ref(var)
   end
   local function prim(op, n)
      return function(...)
         local args = {...}
         assert(#args == n)
         return app(op, args)
      end
   end
   local function close(init, fun)
      local svar = new_state()
      init_state[svar+1] = init
      local sin = ref(svar)
      local sout, out = fun(sin)
      table.insert(code, {'set-state!', svar, sout})
      return out
   end
   local function vec(n, fun)
      -- Variable size arrays are not supported, but really should not
      -- be a big deal if they reside on the stack, which is going to
      -- be the case for most code.
      assert(type(n) == 'number')
      local vec_index = new_var()
      local vec_out   = new_var()
      local saved_code = code ; code = {}
      local el_out = fun(ref(vec_index))
      assert(el_out[1] == 'ref')
      types[vec_out] = {'vec', types[el_out[2]], n}
      table.insert(code,{'vector-set!', vec_out, vec_index, el_out})
      table.insert(code, {'vector-loop!', vec_index, n})
      table.insert(saved_code, {'vector-begin', vec_out, n, code})
      code = saved_code
      return ref(vec_out)
   end
   local c = {
      add   = prim('add',2),
      add1  = prim('add1',1),
      vec   = vec,
      close = close,
   }
   signal_mt.__add = c.add

   local args = {c}
   for i=1,nb_input do
      table.insert(args, signal('input',i))
   end
   local out = { prog(unpack(args)) }

   -- log_desc({code=code,out=out,init=init_state,nb_input=nb_input})
   local function w_expr(w, code)
      if type(code) == 'table' then
         w('( ')
         for _,c in ipairs(code) do
            w_expr(w, c) ; w(' ')
         end
         w(')')
      else
         w(code)
      end
   end
   local tab = ''
   local function w_seq(w, code)
      for _,c in ipairs(code) do
         if c[1] == 'vector-begin' then
            local _, name, n, sub_code = unpack(c)
            w('( vector-begin ', name, " ", n, "\n")
            local saved_tab = tab ; tab = tab .. '    '
            w_seq(w, sub_code)
            tab = saved_tab
            w(')\n')
         else
            w(tab)
            w_expr(w, c)
            w('\n')
         end
      end
   end

   w("\n")
   w("types:\n")
   log_desc(types)
   w("input:\n")
   log_desc({init_state=init_state,nb_input=nb_input})
   w("code:\n")
   w_seq(w, code)
   w("output:\n")
   for _,c in ipairs(out) do
      w_expr(w, c)
      w('\n')
   end

   return code
end

compile(prog3, 1)
compile(prog4, 2)
compile(prog5, 1)
compile(prog2, 1)
compile(prog1, 1)
compile(prog6, 1)
compile(prog7, 1)




-- TODO:
-- . C code generator
-- . Lua (Lure) code generator for LuaJIT
-- . Combinators

