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

-- FIXME: Represenation is crude
local function primitive_type(typ)
   return type(typ) == 'string'
end



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




local function w_c(prog)

   local expr
   local function expr_list(lst)
      local s_lst = {}
      for el, nxt in se.elements(lst) do
         table.insert(s_lst, expr(el))
         if se.is_pair(nxt) then
            table.insert(s_lst, ', ')
         end
      end
      return s_lst
   end
   local function indexed(lst)
      local s = {}
      for el in se.elements(lst) do
         table.insert(s,'[')
         table.insert(s,el)
         table.insert(s,']')
      end
      return s
   end

   function expr(code)
      if is_signal(code) then
         local s_index = ""
         if code.index then
            s_index = indexed(code.index)
         end
         -- code.op can be dropped: both ref and lit are directly represented
         -- return { code.op,"(",code.arg, index, ')' }
         return {code.arg, s_index}
      elseif type(code) == 'table' then
         local op, args = unpack(code)
         local s_args = expr_list(args)
         if op == 'copy' then
            return s_args
         else
            return {op,'(',s_args,')'}
         end
      else
         return code
      end
   end

   local function type_expr(e, var)
      local dims = {}
      local vec, n
      while not primitive_type(e) do
         vec, e, n = se.unpack(e,{n=3})
         assert(vec == 'vec')
         table.insert(dims, n)
      end
      return {e, '_t ', var, indexed(a2l(dims))}
   end


   local function w_expr(w, e)
      w(expr(e))
   end

   local tab = ''

   local function w_seq(w, code)
      for c in se.elements(code) do
         if (se.car(c) == 'for-index') then
            local _, index, n, sub_code = se.unpack(c, {n=4})
            w(tab, 'for(',prog.types[index],'_t ', index, ' = 0; ',
              index, ' < ', n,'; ',index,'++) {\n')
            local saved_tab = tab ; tab = tab .. '  '
            w_seq(w, sub_code)
            tab = saved_tab
            w(tab,'}\n')
         elseif (se.car(c) == 'let') then
            local _, var, expr = se.unpack(c, {n=3})
            w(tab, prog.types[var], '_t ', var, ' = ')
            w_expr(w,expr)
            w(';\n')
         elseif (se.car(c) == 'alloc') then
            -- Technically this is a 2nd pass
            local _, var, typ = se.unpack(c, {n=3})
            if not prog.is_out[var] then
               w(tab, type_expr(typ, var))
               w(';\n')
            end
         elseif (se.car(c) == 'vector-set!') then
            local _, var, idxs, expr = se.unpack(c, {n=4})
            w(tab, var)
            w(indexed(idxs))
            w(" = ")
            w_expr(w,expr)
            w(';\n')
         elseif (se.car(c) == 'state-set!') then
            local _, var, idxs, expr = se.unpack(c, {n=4})
            w(tab, var)
            w(indexed(idxs))
            w(" = ")
            w_expr(w,expr)
            w(';\n')
         elseif (se.car(c) == 'set!') then
            local _, var, expr = se.unpack(c, {n=3})
            w(tab, var)
            w(" = ")
            w_expr(w,expr)
            w(';\n')
         else
            w(tab)
            w_expr(w, c)
            w('\n')
         end
      end
   end


   local function w_prog(w, prog)
      w("\n")
      w("/* types:\n")
      for k,v in pairs(prog.types) do
         w(k,": ")
         w_expr(w, v)
         w("\n")
      end
      w("*/\n")
      local function function_arg(var)
         local typ = prog.types[var]
         if typ ~= nil then
            return type_expr(typ, var)
         else
            return {'type? ', var}
         end
      end

      w("void fun(\n");
      local args={}
      for state in se.elements(prog.state) do
         local var, init = se.unpack(state, {n=2})
         table.insert(args, function_arg(var))
      end
      for var in se.elements(prog.args) do
         table.insert(args, function_arg(var))
      end
      for var in se.elements(a2l(prog.out)) do
         table.insert(args, function_arg(var))
      end
      for arg, nxt in se.elements(a2l(args)) do
         w('  ', arg)
         if se.is_pair(nxt) then
            w(',')
         end
         w('\n')
      end
      w(")\n{\n");
      tab = '  '
      w_seq(w, prog.code)
      tab = ''
      w('}\n')
   end

   w_prog(w, prog)
end

local function compile(hoas, nb_input)
   -- State
   local code = {}
   local new_var_number = counter(1)
   local types = {}
   local state = {}

   -- Current spatial context needs to be tracked for 'rec', as
   -- state needs to be instantiated multiple times inside a spatial
   -- iteration, and set/ref need to be indexed.
   local index  = {}  -- loop index : array(var_name)
   local dims   = {}  -- corresponding dimx : array(integer)
   local vindex = {}  -- vector index, subset of loop index : array(var_name)
   local vecs   = {}  -- vector nesting (each dimension has a ref) : array(refl)
   -- If an index is part of a current iteration, we know the dims.
   local function index_to_dims(idx0)
      for i,idx in ipairs(index) do
         if idx == idx0 then
            return dims[i]
         end
      end
   end

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

   local copy = prim('copy',1)

   local function rec(init_vals, fun)
      -- State needs to be instantiated for each iteration in the
      -- current spatial context.  Determine the type.
      function wrap_type(typ)
         for i=#dims,1,-1 do
            typ = l('vec', typ, dims[i])
         end
         return typ
      end
      local typ = wrap_type("val")


      -- The index used for set/ref
      local svar_ref_index = a2l(index)

      local svars = {}
      local sins  = {}

      -- Here 'pairs' is used so that svars, sins and later souts are
      -- referenced with the same structure as init_vals.
      for key in pairs(init_vals) do
         -- Create variable
         local svar = new_var('s', typ)
         svars[key] = svar
         -- Track it together with the initial value.
         table.insert(state, l(svar, wrap_type(init_vals[key])))
         local svar_ref = ref(svar)
         -- State is always indexed
         svar_ref.index = svar_ref_index
         sins[key] = copy(svar_ref)
      end

      -- Push state inputs into the state machine, collecting next
      -- state and output variables/expressions.
      local souts, out = fun(unpack(sins))

      -- Note that this is pipelined: effect of set! is only visible
      -- at the next iteration.
      for key in pairs(init_vals) do
         table.insert(code, l('state-set!', svars[key], svar_ref_index, souts[key]))
      end
      return out
   end

   local function rec1(init_val, fun)
      return
         rec(
            {init_val},
            function(si)
               local so, o = fun(si)
               return {so}, o
         end)
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
      table.insert(index,  vec_idx)
      table.insert(vindex, vec_idx)
      table.insert(dims,   n)
      table.insert(vecs,   vec_out)

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
         -- Old approach: using explicit pointers
         -- local cset = l('vector-set!', vec_out, vec_idx, el_out))
         -- New approach: index into outer nested array
         local cset = l('vector-set!', vecs[1], a2l(vindex), el_out)
         table.insert(code, cset)
      end

      -- Pop the loop block
      local loop = l('for-index', vec_idx, n, a2l(code))
      code = parent_code

      -- Pop spatial context
      table.remove(index)
      table.remove(vindex)
      table.remove(dims)
      table.remove(vecs)

      -- Introduce the vector name.
      if #dims == 0 then
         -- At the top level of the iteration the storage needs to be
         -- defined.  This is either a C stack allocation, or a
         -- reference to an output provided by the C function's
         -- caller.  This distinction is not known at this time, but
         -- will be in the second pass.
         --
         -- Note that the intermediate vec_out values are still used
         -- for type inference.
         table.insert(
            code,
            l('alloc', vec_out, vec_out_type))
      else
         -- Old approach: use explicit pointer.
         -- This should then be referenced by vector-set!
         -- table.insert(
         --    code,
         --    l('let', vec_out,
         --      l('pointer', vecs[#vecs], index[#index])))
      end
      table.insert(code, loop)

      return ref(vec_out)
   end

   -- Similar to vec(), see comments there.
   -- Differences: no vindex, vecs, vec_out
   local function fold(init_vals, n, fun)
      assert(type(n) == 'number')

      -- Variables used
      local vec_idx = new_var('i','idx')
      local accus = {} -- accumulator var
      local accus_ref = {} -- ref to accumulator var
      for i=1,#init_vals do
         accus[i] = new_var('c','val') -- FIXME: type
         accus_ref[i] = ref(accus[i])
      end

      -- Initialize the loop state
      for i=1,#init_vals do
         table.insert(
            code,
            l('let', accus[i], init_vals[i]))
      end


      -- Push spatial context
      table.insert(index,  vec_idx)
      table.insert(dims,   n)

      -- Push code context
      local parent_code = code ; code = {}

      local accus_copy = {} -- copy of accumulator var (input)

      for i=1,#init_vals do
         accus_copy[i] = copy(accus_ref[i])
      end
      local els_out = {fun(ref(vec_idx), unpack(accus_copy))}

      for i=1,#init_vals do
         assert(els_out[i].op == 'ref')
         local el_out_var = els_out[i].arg
         local el_out_type = types[el_out_var]
         -- FIXME: only primitive types are supported as return values.
         -- Essentially, it is currently not clear how to have vectors as
         -- state variables.
         assert(primitive_type(el_out_type))
         -- FIXME: store the loop iteration variable
         local cset = l('set!', accus[i], els_out[i])
         table.insert(code, cset)
      end

      -- Pop the loop block
      local loop = l('for-index', vec_idx, n, a2l(code))
      code = parent_code

      -- Pop spatial context
      table.remove(index)
      table.remove(dims)

      -- Paste the loop body
      table.insert(code, loop)


      return unpack(accus_ref)
   end

   local function fold1(init_val, n, fun)
      return fold({init_val}, n, function(idx, accu)
            local accu_next = fun(idx, accu)
            return accu_next
      end)
   end



   function aref(sig, ...)
      local args = {...}
      assert(sig.op == 'ref')
      local sig_ref = ref(sig.arg)
      local index = {}
      for i,a in ipairs(args) do
         -- FIXME: This is probably not ok: allow literals
         assert(a.op == 'ref')
         index[i] = a.arg
      end
      -- log_desc({code_index=code.index, s_index=s_index})
      sig_ref.index = a2l(index)

      local t = types[sig.arg]
      if not t then
         local dims = {}
         for i, idx in ipairs(args) do
            dims[i] = index_to_dims(idx.arg)
            -- FIXME: Not valid when i is computed
            assert(dims[i])
         end
         -- FIXME: base type is unknown here, but can be inferred once
         -- the value is used.  Use a different representation for
         -- types so this is easy to patch later.
         local typ = "val"
         for i=#dims,1,-1 do
            typ = se.list('vec', typ, dims[i])
         end
         types[sig.arg] = typ
      else
         -- Typecheck
      end
      return sig_ref
   end

   local c = {
      add   = prim('add',2),
      sub   = prim('sub',2),
      mul   = prim('mul',2),
      add1  = prim('add1',1),
      aref  = aref,
      vec   = vec,
      fold  = fold,
      fold1 = fold1,
      rec   = rec,
      rec1  = rec1,
   }
   -- Metatable operator overloading.
   signal_mt.__add = c.add
   signal_mt.__sub = c.sub
   signal_mt.__mul = c.mul
   -- Implement array access as function call, to support more than
   -- one index.
   signal_mt.__call = aref

   local prog_fun = hoas(c)

   local args = {}
   local arg_refs = {}
   for i=1,nb_input do
      local a = new_var('a')
      table.insert(args, a)
      table.insert(arg_refs, signal('ref', a))
   end
   local outputs = { prog_fun(unpack(arg_refs)) }

   local out_vars = {}
   local is_out = {}
   for i, o in ipairs(outputs) do
      assert(o.op == 'ref')
      out_vars[i] = o.arg
      is_out[o.arg] = true
   end

   -- log_desc({code=code,out=out,init=init,nb_input=nb_input})


   local prog = {
      types  = types,
      state  = a2l(state),
      args   = a2l(args),
      code   = a2l(code),
      out    = out_vars,
      is_out = is_out,
   }

   -- w_scheme(prog)
   w_c(prog)


   return prog
end


return { compile = compile }
