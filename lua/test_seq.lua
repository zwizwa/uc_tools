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

-- It's more convenient to wrap the semantics 'c' around a collection
-- of functions, thant to have a collection of functions with a 'c'
-- parameter, curried or not.
local function progs(c)

   local m = {}
   function m.prog1(i)
      local function counter_sm(s) return s+1, s end
      local counter = c.close(0, counter_sm)
      return counter + i
   end
   function m.prog2()
      function update(s) return c.add1(s), s end
      return c.close(0, update)
   end
   function m.prog3(a)
      return c.add1(a)
   end
   function m.prog4(a, b)
      return c.add(a, b)
   end
   function m.prog5(a)
      return a + 1
   end
   function m.prog6(a)
      return c.vec(3, function(i) return i + 1 end)
   end
   function m.prog7(a)
      local a = c.vec(
         -- Vector size needs to be known at compile time (for now).
         13,
         function(
               -- Index into array
               i,
               -- Dereference for elements already computed.  ref(j)
               -- is valid for j<i.  If this is not used, the
               -- computation is not causally linked so it can be
               -- performed in parallel.
               ref)
            -- The return value is what goes into slot i
            return i + i + 1
      end)
      -- Later, implement auto-lifting of simple constructs.
      return a + 1
   end
   function m.prog8(a)
      return c.vec(13, function(i)
      return c.vec(17, function(j)
      return i + i + 1
      end)
      end)
   end
   return m
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

llp = progs(signal_c)

local f1 = llp.prog1(ll.pure(100))
local f2 = llp.prog2()

log_desc({
      f1 = ll.take(3,f1),
      f2 = ll.take(3,f2)
})

-- FIXME: old api
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


local function counter(n)
   return function()
      local val = n
      n = n + 1
      return val
   end
end


-- SEMANTICS: COMPILER

local function is_signal(thing)
   return type(thing) == 'table' and thing.class == 'signal'
end


-- FIXME: use se pretty printer
local function w_prog(prog)

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

   local function w_expr(w, code)
      if is_signal(code) then
         w('(',code.op," ",code.arg,')')
      elseif se.is_pair(code) then
         w('( ')
         for c in se.elements(code) do
            w_expr(w, c) ; w(' ')
         end
         w(')')
      else
         w(code)
      end
   end
   local tab = ''
   local function w_seq(w, code)
      for c in se.elements(code) do
         if (se.car(c) == 'vector-begin') then
            local _, name, n, sub_code = se.unpack(c, {n=4})
            w(tab, '( vector-begin ', name, " ", n, "\n")
            local saved_tab = tab ; tab = tab .. '    '
            w_seq(w, sub_code)
            tab = saved_tab
            w(tab,')\n')
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
      w("init_state: ")
      w_expr(w, prog.init_state)
      w("\n")
      w("nb_input: ", prog.nb_input, "\n")
      w("code:\n")
      w_seq(w, prog.code)
      w("output:\n")
      for c in se.elements(prog.out) do
         w_expr(w, c)
         w('\n')
      end
   end

   w_prog(w, prog)
end

local function compile(prog_name, nb_input)
   -- State
   local code = {}
   local new_var_number = counter(1)
   local types = {}
   local new_state = counter(1)
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
   local function signal(op, arg)
      local rep = {op = op, arg = arg}
      rep.class = 'signal'
      setmetatable(rep, signal_mt)
      return rep
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
      table.insert(code, se.list('let', var, se.cons(op, se.array_to_list(args))))
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
      init_state[svar] = init
      local sin = ref(svar)
      local sout, out = fun(sin)
      table.insert(code, se.list('set-state!', svar, sout))
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
      assert(el_out.op == 'ref')
      types[vec_out] = se.list('vec', types[el_out.arg], n)
      table.insert(code, se.list('vector-set!', vec_out, vec_index, el_out))
      table.insert(code, se.list('vector-loop!', vec_index, n))
      table.insert(saved_code, se.list('vector-begin', vec_out, n, se.array_to_list(code)))
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

   local c_progs = progs(c)
   local prog_fun = c_progs[prog_name]

   local args = {}
   for i=1,nb_input do
      table.insert(args, signal('input',i))
   end
   local out = { prog_fun(unpack(args)) }

   -- log_desc({code=code,out=out,init=init_state,nb_input=nb_input})

   local prog = {
      types = types,
      init_state = se.array_to_list(init_state),
      nb_input = nb_input,
      code = se.array_to_list(code),
      out = se.array_to_list(out),
   }

   w_prog(prog)


   return prog
end

compile('prog3', 1)
compile('prog4', 2)
compile('prog5', 1)
compile('prog2', 1)
compile('prog1', 1)
compile('prog6', 1)
compile('prog7', 1)
compile('prog8', 1)




-- TODO:
-- . C code generator
-- . Lua (Lure) code generator for LuaJIT
-- . Combinators

