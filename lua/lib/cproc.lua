-- Code for generating cproc patching code.
-- TL;DR
-- 1. Some hand-written loops need special layout (e.g. vector busses)
-- 2. Absorb "transposition" in the fmap function for simple processors


local m = { test = {} }
local iolist = require('lure.iolist')
local list = require('lib.tools.list')
local prefix = iolist.prefix
local postfix = iolist.postfix
local join = iolist.join
local map  = list.map
local imap = list.imap

--
-- After doing some more hands-on work, it really seems that block
-- processing is essential to be able to use the available
-- parallelism:
--
-- 1. vectorization
-- 2. multi-issue
-- 3. pipelining
--
-- These are two _distinct_ constraints.
--
-- Vectorization is fairly straightforward: make sure that data is
-- organized in memory such that it does not need to be permuted on
-- load/store.
--
-- Multi issue can mostly be lumped in with vectorization.  Typically
-- in a DSP core routine you keep the data math unit busy and use
-- multi issue to perform pointer math.
--
-- Pipelining code that accumulates often needs multiple accumulators
-- to avoid stalls due to data dependencies.
--
--
-- An additional constraint is memory bandwidth between cache memory
-- and the registers, and in lesser extent between slower and faster
-- caches.  There are conflicting constraints here: avoiding pipeline
-- stalls means using more registers, which when they run out will
-- require memory transfers.
--
--
-- The point I am trying to make is that to make something fast, the
-- algorithm needs as much parallelism and data-locality as possible,
-- which boils down to:
-- . block processing
-- . input/output/state memory layout optimization
--
-- Assuming the 'bottom' abstraction is the cproc (element-wise
-- processing), we do have a degree of freedom in the loop patterns
-- and data layout.
--
-- This module deals mostly with that.
--
--
-- Summary: how to write optimal DSP code?
-- . write vectorized assembly for the modules with the highest cpu load
-- . create a high level patcher that has 'wires as multi-sample buffers'
-- . generate the buffer transposition code
-- . automate buffer allocation



-- Cases:
--
-- 1: given single-channel op (e.g. EQ), map it over i/o busses that
--    have arbitrary transposition.


-- The core insight is that the buffer layout is a property of the
-- _wire_, and that we deal with that in the 'bus mapper'.
--
-- In the current mixer-style app the parallelism we exploit is the
-- bus: there are a number of signal buffers with filters, and those
-- filters are implemented with core routines that operate on
-- float32x4_t neon code, e.g. 4 busses are bundled into a single
-- buffer.  Other parts of the app have just float buffers.  We are
-- inerested in generating the loop code for those "connecting
-- processors".

-- Given: a scalar processor + i/o buffers with specific layout
-- Asked: generate a loop that connects buffers to scalar proc

-- For that the basic abstraction is the representation of a signal
-- dereference.  This is in the end just a coordinate transform.
--
-- high level:  bus, sample
-- low level:   buffer, offset
--
-- To make this optimizable, the transformer should produce the data
-- necessray to generate an interator:
-- . init code
-- . update code
-- Assume that stop condition is separate

-- Inputs layed out as 4-vector busses


-- I wonder if what I am trying to do is just this:
-- . Keep transposers simple to/from vec4
-- . Generalize wire with 1-element proc
-- . Put all transposers in the schematic explicitly
--
-- It's easy enough to make the vectorization explicit in the toplevel
-- schematic.






-- The core viewpoint is how the input and output buffers look inside
-- a processor's block loop.  Those are what needs to be abstracted.
-- We represent them as a pair: pointer init code, pointer update
-- code.  It could in theory be abstracted as a C function, but let's
-- keep it as concrete as possible to get it going.  Only support
-- float buffers, and only use interleaving.
--
-- float *pin0 = ibuf0 + 1;
-- float *pin1 = ibuf1 + 0;
-- float *pout = obuf + 0;
-- for(int i=0; i<nb; i++) {
--     float in0 = *pin0;
--     float in1 = *pin1;
--     float out = in0 + in1;
--     *pout = out;
--     pout += 4;
--     pin0 += 4;
--     pin0 += 1;
-- }
--
-- For each input, output there is just the offset, stride pair.  Note
-- that because the inner loop uses pointers, the buffer wrapper
-- "patch language" doesn't need to distinguish between input and
-- output ports, so we just drop that here.

-- FIXME: Now do the same but use applicative form anyway, and a
-- struct with named pointers.


-- function m.render_map_cproc_args(cproc, ports, opts)
--    opts = opts or {}
--    local nb = opts.nb or 256

--    local init_code = {}
--    local update_code = {}
--    local indent = '    '
--    local indent2 = {indent, indent}
--    local typed_busses   = {}
--    local channels = {}
--    for i,port in ipairs(ports) do
--       local chan = {'chan_',i-1}
--       local bus  = {'bus_', i-1}
--       local typ  = port.typ
--       table.insert(channels, chan)
--       table.insert(typed_busses, {typ, '* ', bus})
--       table.insert(
--          init_code,
--          -- A bus contains multiple channels
--          {indent,typ,' *',chan,' = ',bus,' + ',port.offset,';\n'})
--       table.insert(
--          update_code,
--          {indent2,'*',chan,' += ', port.stride, ';\n'})
--    end
--    return {
--       {'void ',cproc,'_buf(',cproc,'_t *',cproc,'_s, ',join(', ',typed_busses),') {\n'},
--       init_code,
--       {indent,'for(int i=0; i<',nb,'; i++) {\n'},
--       {indent2, cproc, '(', cproc,'_s, ', join(', ',channels), ');\n'},
--       update_code,
--       {indent,'}\n'},
--       {'}\n'},
--    }
-- end
-- function m.test.render_map_cproc_args()
--    local ports = {
--       {typ = 'int',   offset = 1, stride = 4},
--       {typ = 'float', offset = 3, stride = 4},
--       {typ = 'float', offset = 0, stride = 1},
--    }
--    local code = m.render_map_cproc_args('proc1', ports, {nb = 128})
--    iolist.w(code)
-- end


-- Similar, but use a struct.

-- This doesn't use full overengineered cproc api which seems overkill
-- in my current application and I forgot most of how that worked.
-- Later though it would be nice to generalize this to also combine
-- generic cproces with strided buffer management.

-- I do not want to create complication, so assume that each cproc has
-- a "dataflow variable" update form: (state, *in, ... *out, ...)
-- which is named _update_df

-- Suffixes used
-- _state     state struct
-- _update_df update function in dataflow varaible form
-- _node      block processing node as it shows up in the graph (state + io)
-- _loop      the loop over input/output blocks


function m.render_loop(cproc, ports, opts)
   opts = opts or {}
   local state = opts.state or '&s->state'

   local init_code = {}
   local update_code = {}
   local indent = '    '
   local indent2 = {indent, indent}
   local typed_busses   = {}
   local channels = {}
   for i,port in ipairs(ports) do
      local chan = {'p',i-1}
      local bus  = {'s->',port.name}
      local typ  = port.typ
      table.insert(channels, chan)
      table.insert(typed_busses, {typ, '* ', bus})
      table.insert(
         init_code,
         -- A bus contains multiple channels
         {indent,typ,' *',chan,' = ',bus,' + ',port.offset,';\n'})
      table.insert(
         update_code,
         {indent2, chan,' += ', port.stride, ';\n'})
   end
   return {
      {'static void ',cproc,'_loop(struct ',cproc,'_node *s, uintptr_t nb) {\n'},
      init_code,
      {indent,'for(uintptr_t i=0; i<nb; i++) {\n'},
      {indent2, cproc, '_update_df(',state,', ', join(', ',channels), ');\n'},
      update_code,
      {indent,'}\n'},
      {'}\n'},
   }
end
function m.test.render_loop()
   local ports = {
      {name = 'input.in1',  typ = 'int',   offset = 1, stride = 4},
      {name = 'input.in2',  typ = 'float', offset = 3, stride = 4},
      {name = 'output.out', typ = 'float', offset = 0, stride = 1},
   }
   local code = m.render_loop('proc1', ports, {nb_samples = 256})
   iolist.w(code)
end


function m.render_nop(cproc)
   return {'static void ',cproc,'_loop(struct ',cproc,'_node *s, uintptr_t nb) {}\n'}
end




-- CPROC combinators.
-- The idea is to mimick two combinators used in sigmastudio:
-- 1. Parallel filter, delay blocks, sharing coefficients
-- 2. Sequential filter blocks like equalizers, joining coefficients

function ifndef_guard(name)
   return {
      '#ifndef ',name,'_cproc\n',
      '#define ',name,'_cproc\n',
   }
end


function m.parallel(env, spec, size)
   assert(spec.name)
   assert(spec.ins)
   assert(spec.outs)
   assert(spec.params)
   assert(size)
   local indent = '    '
   local indent2 = {indent, indent}
   local struct_members = {}
   local pname = spec.name .. '_p' .. size
   local struct = { 'struct ',pname, '_state' }
   local spec_ios = list.concat({spec.ins, spec.outs})
   local ios = {}
   for _,input in ipairs(spec_ios) do
      for i=0,size-1 do
         table.insert(ios, {',\n',indent,'float *', input, i})
      end
   end
   local function pio_n(n)
      return iolist.join(', ', postfix(spec_ios,n))
   end
   local body= {}
   for i=0,size-1 do
      local state = {'&s->',spec.name,'[',i,']'}
      table.insert(body, {indent, spec.name, '_update_df(',state,', ', pio_n(i),');\n'})
   end

   local setters = {}
   for _, param_spec in ipairs(spec.params) do
      local param, typ = unpack(param_spec)
      table.insert(
         setters, {
            'static inline void ',pname,'_set_',param,'(',struct,' *s, ',typ,' v) {\n',
            indent, 'for(int i=0; i<',size,'; i++) ',spec.name,'_set_',param,'(&s->',spec.name,'[i], v);\n',
            '}\n',
      })
   end

   local inits = {
      'static inline void ',pname,'_init(',struct,' *s) {\n',
      indent, 'for(int i=0; i<',size,'; i++) ',spec.name,'_init(&s->',spec.name,'[i]);\n',
      '}\n',
   }

   local code = {
      '// parallel ', spec.name, '\n',
      ifndef_guard(pname),
      struct,' {\n',
      indent,'struct ',spec.name, '_state ', spec.name, '[',size,'];\n',
      '};\n',
      'static inline void ',pname,'_update_df(\n',
      indent, struct, ' *s', ios, ')\n{\n',
      body,
      '}\n',
      setters,
      inits,
      '#endif\n',
   }
   local new_spec = {
      name = pname,
      ins  = {},
      outs = {},
      params = spec.params,
   }
   for i=1,size do
      for _, input in ipairs(spec.ins) do
         table.insert(new_spec.ins,  input .. '_' .. i)
      end
      for _, output in ipairs(spec.outs) do
         table.insert(new_spec.outs, output .. '_' .. i)
      end
   end


   -- It's simpler to accumulate specs in an environment.
   env[new_spec.name] = new_spec

   return code
end

m.spec = {}
m.spec.lowpass = {
   name   = 'lowpass',
   ins    = {'in'},
   outs   = {'out'},
   params = {{'freq','float'}},
}
m.spec.highpass = {
   name   = 'highpass',
   ins    = {'in'},
   outs   = {'out'},
   params = {{'freq','float'}},
}

function m.test.parallel()
   local env = {}
   local spec = m.spec.lowpass
   local code = m.parallel(env, spec, 3)
   log_desc({env=env})
   iolist.w(code)
end

-- Transformed parameters
function m.serial_params(specs)
   local params = {}
   for i,spec in ipairs(specs) do
      for _,param_spec in ipairs(spec.params) do
         local param, typ = unpack(param_spec)
         table.insert(params, {param .. '_' .. i, typ})
      end
   end
   return params
end

-- FIXME: For now this only works for single-channel procs
function m.serial(env, specs, maybe_name)
   local nb = #specs
   assert(nb > 0)
   local names = {}
   local substruct = {}
   for i,spec in ipairs(specs) do
      assert(spec.name)
      assert(spec.ins)
      assert(spec.outs)
      assert(spec.params)
      names[i] = spec.name
   end
   local comb_name = maybe_name or iolist.to_string(join('_', names))
   local struct = {'struct ', comb_name, '_state'}
   local indent = '    '
   local indent2 = {indent, indent}
   function substruct_def(i, spec)
      return {indent,'struct ',spec.name, '_state ',spec.name,i,';\n'}
   end
   -- Connectivity
   local tmp = {}
   for i=1,nb-1 do table.insert(tmp, {indent,'float tmp',i,';\n'}) end
   function sub_in(i)
      if i==1 then return "in" else return {'&tmp',i-1} end
   end
   function sub_out(i)
      if i==nb then return "out" else return {'&tmp',i} end
   end
   function update(i, spec)
      return {indent, spec.name, '_update_df(&s->',spec.name,i,', ',sub_in(i),', ',sub_out(i),');\n'}
   end

   local setters = {}

   for i,spec in ipairs(specs) do
      for _, param_spec in ipairs(spec.params) do
         local param, typ = unpack(param_spec)
         table.insert(
            setters, {
               -- Use just the index in the setter name
               'static inline void ',comb_name,'_set_',param,'_',i,'(',struct,' *s, ',typ,' v) {\n',
               indent, spec.name,'_set_',param,'(&s->',spec.name,i,', v);\n',
               '}\n',
         })
      end
   end

   local function call_init(i, spec)
      return {indent, spec.name,'_init(&s->',spec.name,i,');\n',}
   end
   local inits = {
      'static inline void ',comb_name,'_init(',struct,' *s) {\n',
      imap(call_init, specs),
      '}\n',
   }

   local code = {
      '// serial ', join('->', names), '\n',
      ifndef_guard(comb_name),
      struct,' {\n',
      imap(substruct_def, specs),
      '};\n',
      'static inline void ',comb_name,'_update_df(',struct,'*s, float *in, float *out) {\n',
      tmp,
      imap(update, specs),
      '}\n',
      setters,
      inits,
      '#endif\n',
   }
   local new_spec = {
      name = comb_name,
      ins  = specs[1].ins,
      outs = specs[nb].outs,
      params = m.serial_params(specs)
   }

   -- It's simpler to accumulate specs in an environment.
   env[new_spec.name] = new_spec

   return code

end

function m.serial_many(env, spec, n)
   local specs = {}
   for i=1,n do table.insert(specs, spec) end
   return m.serial(env, specs, spec.name .. '_s' .. n)
end



function m.test.serial()
   local specs = {
      m.spec.highpass,
      m.spec.lowpass,
      m.spec.lowpass,
   }
   local env = {}
   local code = m.serial(env, specs)
   log_desc({env=env})
   iolist.w(code)
end



-- FIXME: These should also transform the spec at the same time of course.




return m
