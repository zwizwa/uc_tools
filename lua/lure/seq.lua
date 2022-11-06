-- Compile squential language expressed as HOAS to S-Expression
-- See test_seq.lua

-- The first pass in the transformation implements time as state
-- feedback, and space as vector element init loops.  Input is HOAS
-- and output is S-Expression syntax with explicit mutation for state
-- and vectors, but value semantics for return values.
--
-- The second pass transforms value semantics to "return location
-- passing" for time state and space vectors.


local se = require 'lure.se'
local a2l = se.array_to_list
local l = se.list
require 'lure.log'

local function counter(n)
   return function()
      local val = n
      n = n + 1
      return val
   end
end


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
      w("state: ")
      w_expr(w, prog.state)
      w("\n")
      w("args: ")
      w_expr(w, prog.args)
      w("\n")
      w("code:\n")
      w_seq(w, prog.code)
      w("return:\n")
      for c in se.elements(prog.out) do
         w_expr(w, c)
         w('\n')
      end
   end

   w_prog(w, prog)
end

-- FIXME: Represenation is crude
local function primitive_type(typ)
   return type(typ) == 'string'
end

local function compile(hoas, nb_input)
   -- State
   local code = {}
   local new_var_number = counter(1)
   local types = {}
   local state = {}

   -- Current spatial context needs to be tracked for 'close', as
   -- state needs to be instantiated multiple times inside a spatial
   -- iteration, and set/ref need to be indexed.
   local index = {}
   local dims  = {}
   local vecs  = {}

   local function new_var(prefix, typ)
      assert(prefix)
      local v = prefix .. new_var_number()
      -- type might be unknown at this point, typ=nil is allowed
      types[v] = typ
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
      local var = new_var('r','val')
      local args = {}
      for i,a in ipairs(args_) do args[i] = project(a, lit) end
      table.insert(code, l('let', var, se.cons(op, a2l(args))))
      return ref(var)
   end

   local function prim(op, n)
      return function(...)
         local args = {...}
         assert(#args == n)
         return app(op, args)
      end
   end

   local function close(init_val, fun)
      -- State needs to be instantiated for each iteration in the
      -- current spatial context.  Determine the type.
      local typ = "val"
      for i=#dims,1,-1 do
         typ = l('vec', typ, dims[i])
      end
      -- The index used for set/ref
      local sindex = a2l(index)

      -- Create variable, and track it together with the initial value.
      local svar = new_var('s', typ)
      table.insert(state, l(svar, init_val))

      -- Bind the reference to the current index and push it into the
      -- state machine, collecting next state and output
      -- variables/expressions.
      local sin = ref(svar)
      sin.index = sindex
      local sout, out = fun(sin)

      -- Note that this is pipelined: effect of set! is only visible
      -- at the next iteration.
      table.insert(code, l('state-set!', svar, sindex, sout))
      return out
   end

   local function vec(n, fun)
      -- Variable size arrays are not supported, but really should not
      -- be a big deal if they reside on the stack, which is going to
      -- be the case for most code.
      assert(type(n) == 'number')

      -- Variables used
      local vec_idx = new_var('i','idx')
      local vec_out = new_var('v', nil)

      -- Push spatial context
      table.insert(index, vec_idx)
      table.insert(dims,  n)
      table.insert(vecs,  vec_out)

      -- Push code context
      local parent_code = code ; code = {}

      local el_out = fun(ref(vec_idx))

      -- FIXME: It doesn't make much sense for this to be a 'lit', but
      -- if this ever happens, then wrap it in a var.  For now assume
      -- 'ref'.
      assert(el_out.op == 'ref')
      local el_out_var = el_out.arg
      local el_out_type = types[el_out_var]

      -- The type of the vector is known after evaluation.
      local vec_out_type = l('vec', el_out_type, n)
      types[vec_out] = vec_out_type


      -- Body ends with a store to the current hole.  This only
      -- happens in the inner loop where el_out is an element.
      if primitive_type(el_out_type) then
         table.insert(
            code, l('vector-set!', vec_out, vec_idx, el_out))
      end

      -- Pop the loop block
      local loop = l('for-index', vec_idx, n, a2l(code))
      code = parent_code

      -- Pop spatial context
      table.remove(index)
      table.remove(dims)
      table.remove(vecs)

      -- Introduce the vector name.
      if #dims == 0 then
         -- At the top level of the iteration the storage needs to be
         -- defined.  This is either a C stack allocation, or a
         -- reference to an output provided by the C function's
         -- caller.  This distinction is not known at this time, but
         -- will be in the second pass.
         table.insert(
            code,
            l('vector-alloc', vec_out, vec_out_type))
      else
         -- In an interation context: dereference the outer vector.
         table.insert(
            code,
            l('let', vec_out,
              l('pointer', vecs[#vecs], index[#index])))
      end
      table.insert(code, loop)

      return ref(vec_out)
   end

   local c = {
      add   = prim('add',2),
      add1  = prim('add1',1),
      vec   = vec,
      close = close,
   }
   signal_mt.__add = c.add

   local prog_fun = hoas(c)

   local args = {}
   local arg_refs = {}
   for i=1,nb_input do
      local a = new_var('a')
      table.insert(args, a)
      table.insert(arg_refs, signal('ref', a))
   end
   local out = { prog_fun(unpack(arg_refs)) }

   -- log_desc({code=code,out=out,init=init,nb_input=nb_input})

   local prog = {
      types = types,
      state = a2l(state),
      args = a2l(args),
      code = a2l(code),
      out = a2l(out),
   }

   w_prog(prog)


   return prog
end


return { compile = compile }
