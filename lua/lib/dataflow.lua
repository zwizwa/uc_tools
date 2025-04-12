#!./lua.sh

-- Generate readable dataflow schematics and patching C code from a
-- Lua-embedded 'final' DSL.

-- DF1: Allow for disconnected (zero) inputs.
-- DF2: Reuse buffers when possible.


require('lib.tools.log')
local list   = require('lib.tools.list')
local iolist = require('lure.iolist')
local map    = list.map
local concat = list.concat
local cproc  = require('lib.cproc')

-- The 'w_' functions take iolist and write to stdout
-- The 'render_' functions will render to iolist

local w    = iolist.w
local join = iolist.join



-- RENDER GRAPH TO IOLIST

local function render_ports(s)
   return function (ports_list)
      local port_labels = map(function(p) return {"<",p,">",p} end, ports_list)
      local joined = join("|", port_labels)
      return {"{",joined,"}"}
   end
end

local function render_node(s)
   return function(n)
      local name = n.name
      local i = n.in_ports
      local o = n.out_ports
      assert(name)
      assert(i)
      assert(o)
      return {
         '    ',
         name,'[label="{ ',
         render_ports(s)(i),'|',name, '|',render_ports(s)(o),
         ' }"];\n'
      }
   end
end

local function map_edge(f, e)
   -- log_desc({map_edge=e})
   local from, to = unpack(e)
   assert(from)
   assert(to)
   local from_node, from_port = unpack(from)
   assert(from_node)
   assert(from_port)
   local to_node, to_port = unpack(to)
   assert(to_node)
   assert(to_port)
   return f(from_node, from_port, to_node, to_port)
end

local function render_edge(s)
   return function(e)
      return map_edge(
         function(from_node, from_port, to_node, to_port)
            return {
               '    ',
               from_node,':',from_port,
               ' -> ',
               to_node,':',to_port,
               ';\n'
            }
         end,
         e)
   end
end


-- https://stackoverflow.com/questions/7922960/block-diagram-layout-with-dot-graphviz
local function render_dot(s)
   assert(s.nodes)
   assert(s.edges)
   return {[[
digraph G {
    # splines = false;
    splines = line;
    graph [rankdir = LR];
    node[shape=record];
]],
map(render_node(s), s.nodes),
map(render_edge(s), s.edges),[[
}
]]}
end

local function w_dot(s, maybe_filename)
   w(render_dot(s), maybe_filename)
end


-- RENDER GRAPH TO C CODE


-- Represent the edges in reverse as a nested Lua struct:
-- [input][port] = {output,port}
local function input_edges(s)
   local tab = {}
   local function input(name)
      local itab = tab[name] or {}
      tab[name] = itab
      return itab
   end
   for _,edge in ipairs(s.edges) do
      -- log_desc({edge=edge})
      map_edge(
         function(from_node, from_port, to_node, to_port)
            local itab = input(to_node)
            itab[to_port] = {from_node, from_port}
         end, edge)
   end
   return tab
end

-- TODO:
-- First pass performs some analysis used in the code gen pass.
-- . Buffer reference counts
-- . Null input/output

-- Note that these need to be strings because they are used as table indices.
local function in_buf(node_name, port_name)
   return table.concat({'s->', node_name, '.input.', port_name })
end
local function out_buf(node_name, port_name)
   return table.concat({'s->', node_name, '.output.', port_name })
end

local function analyze(s)
   -- Index the edges by inputs.
   local edge = input_edges(s)

   -- Connections
   local connect = {}

   -- Output buffer stats
   local buf = {}
   local function nop() end

   local did_type = {}

   -- For all processing nodes
   function for_buf(on_input_buf, on_output_buf)
      for _,node in ipairs(s.nodes) do
         local type_name = node.type_name or node.extern_name
         assert(type_name)
         assert(node.name)
         assert(node.in_ports)
         assert(node.out_ports)
         for i,node_in_port in ipairs(node.in_ports) do
            local from = edge[node.name][node_in_port]
            if from then
               local from_node, from_port = unpack(from)
               local from_node_out_buf = out_buf(from_node, from_port)
               on_input_buf(from_node_out_buf)
               local node_in_buf = in_buf(node.name, node_in_port)
               connect[node_in_buf] = from_node_out_buf -- [1]
            end
         end
         for i,node_out_port in ipairs(node.out_ports) do
            local node_out_buf = out_buf(node.name, node_out_port)
            on_output_buf(node_out_buf)
         end

         if not did_type[type_name] then -- [2]
            did_type[type_name] = true
         end

         -- [1][2] The connect and type info is collected on the side
         -- since we are traversing anyway.

      end
   end

   -- Pass1: get usage count
   function pass1_buf_reference(buf_var)
      local tab = buf[buf_var]
      if tab then
      else
         error('buf not defined')
      end
      -- log_desc(buf_var)
      tab.total_use = tab.total_use + 1
   end
   function pass1_buf_alloc(buf_var)
      -- First pass: simple alloc
      assert(nil == buf[buf_var])
      buf[buf_var] = { total_use = 0 }
   end
   for_buf(pass1_buf_reference, pass1_buf_alloc)


   -- Pass2: allocate.
   function buf_alloc_reuse(buf_var)
      -- Second pass: alloc from reuse buffer.
      assert(buf[buf_var].total_use)
   end

   local buf_next = 1   -- buffer 0 is reserved as null out
   local buf_stack = {} -- reusable

   local use_stat = {} -- statistics

   function pass2_buf_reference(buf_var)
      local b = buf[buf_var]
      assert(b)
      assert(b.buf_nb)
      b.ref_count = b.ref_count - 1
      if b.ref_count == 0 then
         -- Move buffer to free stack
         table.insert(buf_stack, b.buf_nb)
      end
   end
   function pass2_buf_alloc(buf_var)
      -- First pass: simple alloc
      local b = buf[buf_var]
      assert(b)
      b.ref_count = b.total_use
      if #buf_stack == 0 then
         -- Allocate a new one
         b.buf_nb = buf_next
         buf_next = buf_next + 1
         use_stat[b.buf_nb] = 1
      else
         -- Reuse
         b.buf_nb = table.remove(buf_stack)
         assert(use_stat[b.buf_nb])
         use_stat[b.buf_nb] = use_stat[b.buf_nb] + 1
      end

   end

   for_buf(pass2_buf_reference, pass2_buf_alloc)

   local types = {}
   for k in pairs(did_type) do table.insert(types, k) end

   local rv = {buf=buf,connect=connect,use_stat=use_stat,edge=edge,types=types}
   -- log_desc(rv)
   return rv

end

-- Render the patching code: structs and
local function render_c(s, graph_name)
   assert(graph_name)

   -- Per type
   local struct_code = {}
   local process_code = {}
   local macros = {}

   -- Per instance
   local graph_struct_code = {}
   local alloc_code = {}
   local null_code = {}
   local connect_code = {}
   local init_code = {}
   -- Index the edges by inputs.
   local edge = input_edges(s)
   -- log_desc({edge=edge})

   local indent = '    '
   local indent2 = {indent, indent}

   local alloc_count = 1

   local function str(n) return n .. '' end

   local did_type = {}

   local null_out = {}

   -- First pass
   for _,node in ipairs(s.nodes) do
      -- Get the processor definition
      -- log_desc({node=node})
      local type_name = node.type_name or node.extern_name
      assert(type_name)
      assert(node.name)
      assert(node.in_ports)
      assert(node.out_ports)

      local out_struct = {}
      for i,out in ipairs(node.out_ports) do
         table.insert(
            out_struct,
            {indent2,'float *',out,';\n'})
      end
      local in_struct = {}


      -- Go over all outputs and mark them for initialization.
      for i,node_out_port in ipairs(node.out_ports) do
         local node_out_buf = out_buf(node.name, node_out_port)
         null_out[node_out_buf] = true
      end

      for i,node_in_port in ipairs(node.in_ports) do

         -- log_desc({node.name,node_in_port})
         local from = edge[node.name][node_in_port]

         table.insert(
            in_struct,
            {indent2,'float *',node_in_port,';\n'})

         local to_input_buf = in_buf(node.name, node_in_port)

         -- Each input comes from some other node's output.
         if from then
            assert(from)
            local from_node, from_port = unpack(from)
            local from_output_buf = out_buf(from_node, from_port)
            -- log_desc({from_node=from_node})
            local f_node = s.node_by_name[from_node]
            -- log_desc({f_node=f_node})
            table.insert(
               alloc_code, {
                  {indent, from_output_buf, ' = s->buf[',str(alloc_count),']',';\n'},
            })
            null_out[from_output_buf] = false

            alloc_count = alloc_count + 1,

            table.insert(
               connect_code, {
                  {indent, to_input_buf, ' = ', from_output_buf, ';\n'}
            })
         -- Or it is not connected.
         else
            -- This is currently an error.  Create a dummy source in
            -- the schematic instead.
            log_desc({not_connected={node.name,node_in_port}})
            error('input not connected')

         end
      end

      -- Add instance to the graph struct
      table.insert(
         graph_struct_code,
         {indent, 'struct ', type_name, '_node ', node.name, ';\n'})

      -- Add instance init
      -- FIXME: Maybe better to assert(node.init) instead of allowing zero defaults.
      if node.init then
         local state = {'&s->',node.name,'.state'}
         table.insert(init_code, {indent, type_name,'_init(',state,');\n'})
         for name, value in pairs(node.init) do
            table.insert(init_code, {indent, type_name,'_set_',name,'(',state,', ',value,');\n'})
         end
      else
         log('WARNING: no init for ' .. node.name .. '\n')
      end

      -- Run the processor instance, which will put the outputs in the
      -- correct place.
      table.insert(
         process_code,
         {indent, type_name, '_loop(&s->',node.name,', nb);\n'})

      -- Per-type struct and process only need to be done once
      assert(type_name)
      if not did_type[type_name] then
         did_type[type_name] = true

         -- Save the data structure
         --
         -- Add guard to avoid multiple definitions when including
         -- multiple graph files.
         table.insert(
            struct_code,
            {'#ifndef struct_', type_name, '_node\n',
             '#define struct_', type_name, '_node\n',
             'struct ', type_name, '_node {\n',
             {indent,'struct {\n', in_struct,  indent,'} input;\n'},
             {indent,'struct {\n', out_struct, indent,'} output;\n'},
             {indent, 'struct ', type_name, '_state state;\n'},
             '};\n',
             '#endif\n'})

         -- Save macros
         --
         -- Note that macros are not in the guard.  As a side effect
         -- this will check that the inputs are consistent across
         -- multiple graph files because macros will get redefined and
         -- C preprocessor will not complain if definition is the
         -- same.
         function def_macro(tag,ports)
            table.insert(macros, {'#define for_',type_name,'_',tag,'(m) \\\n'})
            for i,input in ipairs(ports) do
               table.insert(macros, {indent, 'm(',i-1,',',input,') \\\n'})
            end
            table.insert(macros, {'\n'})
         end
         def_macro('inputs',  node.in_ports)
         def_macro('outputs', node.out_ports)
      end

   end

   -- Second pass
   -- log_desc({null_out=null_out})
   for _,node in ipairs(s.nodes) do
      -- Initialize all unused outputs to the null sink.
      for i,node_out_port in ipairs(node.out_ports) do
         local node_out_buf = out_buf(node.name, node_out_port)
         if null_out[node_out_buf] then
            table.insert(
               null_code, {
                  {indent, node_out_buf, ' = s->buf[0]',';\n'},
            })
         end
      end
   end

   table.insert(
      graph_struct_code,
      {indent, 'float buf[',str(alloc_count),'][256];\n'});

   -- Naming is unfortunate, but it is the output port struct of the
   -- input node, and the input port struct of the output node.
   local base_in  = 's->input.output'
   local base_out = 's->output.input'
   local base_code = {
         {indent, 's->base.in      = (float**)&',base_in, ';\n'},
         {indent, 's->base.out     = (float**)&',base_out,';\n'},
         {indent, 's->base.nb_in   = sizeof(',base_in, ')/sizeof(float*);\n'},
         {indent, 's->base.nb_out  = sizeof(',base_out,')/sizeof(float*);\n'},
         {indent, 's->base.process = (graph_base_process_fn)',graph_name,'_graph_process;\n'},
   }



   return {
      macros = macros,
      decl = {
         'struct ',graph_name,'_graph;\n',
         'void ',graph_name,'_graph_init(struct ',graph_name,'_graph *s);\n',
         'void ',graph_name,'_graph_process(struct ',graph_name,'_graph *s, uintptr_t nb);\n',
      },
      structs = {
         struct_code,
         {'struct ',graph_name,'_graph {\n',
          -- FIXME: This should not mess up state alignment.
          {indent, "struct graph_base base;\n"},
          graph_struct_code,
          "};\n"},
      },
      connect = {
         'static const struct param *',graph_name,'_root(void);\n',
         'void ',graph_name,'_graph_init(struct ',graph_name,'_graph *s) {\n',
         {indent, 'memset(s,0,sizeof(*s));\n'},
         {indent, 's->base.pc.root = ',graph_name,'_root()->cont.list;\n'},
         {indent, '// null\n'},
         null_code,
         {indent, '// alloc\n'},
         alloc_code,
         {indent, '// connect\n'},
         connect_code,
         {indent, '// connect\n'},
         init_code,
         {indent, '// base\n'},
         base_code,
         '}\n'},
      process = {'void ',graph_name,'_graph_process(',
                 'struct ',graph_name,'_graph *s, ',
                 'uintptr_t nb) {\n', process_code, '}\n'},
   }
end

-- Generate block-level _process method for a scalar cproc _update_df
-- This only needs to be done once per type as everything is generic.
function render_c_loop(graph, node)
   assert(node.type_name)
   local code = { '// generic cproc ', node.type_name,'\n' }

   local nop = {input = true, output = true}

   if nop[node.type_name] then
      return {code, cproc.render_nop(node.type_name)}
   end

   local function cfg_port(sub)
         return function (p)
            -- FIXME: Get types from graph
            -- return {name = {sub,'.',p}, typ = 'float', offset = 1, stride = 4}
            return {name = {sub,'.',p}, typ = 'float', offset = 0, stride = 1}
         end
   end
   local in_ports  = map(cfg_port('input'),  node.in_ports)
   local out_ports = map(cfg_port('output'), node.out_ports)
   local ports = list.concat({in_ports, out_ports})
   -- log_desc({ports=ports})
   table.insert(code,
                cproc.render_loop(
                   node.type_name,
                   ports,
                   {nb = 256}))
   return code
end




-- COMPILE LUA TO GRAPH


-- To keep it simple: use unique node names at first.
-- How to specify the number of outputs that a processor produces?
-- This should be part of the type definition.

local function graph_compiler()

   local c = { }

   function c:app(typ, name, ...)
      assert(typ)
      assert(type(name) == 'string')
      local ins = {...}
      assert(type(ins) == 'table')

      -- The inputs are always outputs of other nodes.  What we do
      -- here is split that into explicit input port names, and
      -- explicit edges.

      local instance = typ(self, name, #ins)
      assert(type(instance) == 'table')
      local input_name = instance.input_name
      local in_port_names
      if input_name == nil then
         -- No inputs
         in_port_names = {}
      elseif type(input_name) == 'function' then
         -- Variable number of inputs
         in_port_names = {}
         for i,input in ipairs(ins) do
            in_port_names[i] = input_name(i)
         end
      elseif type(input_name) == 'table' then
         -- Fixed number of inputs with names
         in_port_names = map(function(i) return i end, input_name) -- copy
      else
         error('bad input_name type')
      end

      for i,input in ipairs(ins) do
         table.insert(self.edges, {input, {name, in_port_names[i]}})
      end

      instance.name = name
      instance.in_ports = in_port_names
      assert(instance.out_ports)
      table.insert(self.nodes, instance)
      self.node_by_name[name] = instance

      local rvs = map(function(o) return {name, o} end, instance.out_ports)
      return unpack(rvs)
   end


   local state = {
      -- The Graphviz node and edge sets.
      nodes = {},
      edges = {},
      -- Index
      node_by_name = {},
   }
   setmetatable(state, {__index = c})
   return state
end
local function graph_compile(prog, make_ins)
   local c = graph_compiler()
   local outs = prog(c, c:app(make_ins, 'input'))
   return c
end


-- The meaning of a type is a Lua function that instantiates a processor
local t = {}

local function assert_nonneg(n)
   assert(type(n) == 'number')
   assert(n >= 0)
end
local function assert_string(n)
   assert(type(n) == 'string')
end


-- FIXME: Get rid of this one.
function t.bus_op(cproc_name, bus_type)
   return function (c, name, nb_inputs)
      local outs = {}
      for i=1,nb_inputs do outs[i] = 'o' .. i end
      return {
         type_name = cproc_name,
         out_ports = outs,
         input_name = function(i) return 'i' .. i end,
         bus_type = bus_type
      }
   end
end

function t.extern_op(extern_spec, init)
   local extern_name, extern_nb_in, extern_nb_out = unpack(extern_spec)
   assert_string(extern_name)
   assert(extern_name)
   assert_nonneg(extern_nb_in)
   assert_nonneg(extern_nb_out)

   return function (c, name, nb_inputs)
      if nb_inputs ~= extern_nb_in then
         log_desc({name=name, nb_inputs=nb_inputs, extern_nb_in=extern_nb_in})
         error('bad nb_inputs')
      end
      local outs = {}
      for i=1,extern_nb_out do outs[i] = 'o' .. i end
      return {
         extern_name = extern_name,
         init = init,
         out_ports = outs,
         input_name = function(i) return 'i' .. i end,
      }
   end
end



function t.cproc_op(cproc_spec, init)
   local cproc_name, cproc_nb_in, cproc_nb_out = unpack(cproc_spec)
   assert_string(cproc_name)
   assert(cproc_name)
   assert_nonneg(cproc_nb_in)
   assert_nonneg(cproc_nb_out)

   return function (c, name, nb_inputs)
      if nb_inputs ~= cproc_nb_in then
         log_desc({name=name, nb_inputs=nb_inputs, cproc_nb_in=cproc_nb_in})
         error('bad nb_inputs')
      end
      local outs = {}
      for i=1,cproc_nb_out do outs[i] = 'o' .. i end
      return {
         type_name = cproc_name,
         out_ports = outs,
         input_name = function(i) return 'i' .. i end,
         init = init,
      }
   end
end


-- FIXME: This is a special generated one.
function t.matrix_op(type_name, in_names, out_names)
   return function (c, name, nb_inputs)
      return {
         type_name  = type_name,
         out_ports  = out_names,
         input_name = in_names,
      }
   end
end
function t.extern_matrix_op(type_name, in_names, out_names)
   return function (c, name, nb_inputs)
      return {
         extern_name  = type_name,
         out_ports  = out_names,
         input_name = in_names,
         init = {},
      }
   end
end



-- parameterized type
function t.input(n)
   return function (c, name, n0)
      assert(n0 == 0)
      local outs = {}
      if type(n) == 'number' then
         for i=1,n do outs[i] = 'ch' .. i end
      elseif type(n) == 'table' then
         for i,name_i in ipairs(n) do outs[i] = name_i end
      else
         error('bad t.input type ' .. type(n))
      end
      return {
         name = name,
         type_name = 'input',
         out_ports = outs,
         in_ports = {},
      }
   end
end


-- Some utility functions
local function named(names, values)
   local tab = {}
   for i,v in ipairs(values) do
      tab[names[i]] = v
   end
   return tab
end






-- TEST

-- Test for lua to graph compiler
local function test2()
   local t_filter = t.bus_op('fir')
   local function prog1(s, a)
      local b    = s:app(t_filter, 'f1', a)
      local c, d = s:app(t_filter, 'f2', a, b )
      return b
   end
   local function prog2(s, a, b)
      local c, d       = s:app(t_filter, 'f1', a, b )
      local e, f, g, h = s:app(t_filter, 'f2', a, b, c, d )
      return b
   end
   local sch = graph_compile(prog2, t.input(2))
   log_desc(sch)
   w_dot(sch,'test2.dot')
   w_c(sch)

   -- FIXME: C compiler
   -- local code = c_compile(prog2)
   -- w(code)

end


local schematic = {
   nodes = {
      {'input',{},{ 'ch1','ch2','ch3','ch4' }},
      {'fir4',
       {'in1','in2','in3','in4' },
       { 'out1','out2','out3','out4'}},
      {'eq',
       { 'in1','in2'},
       { 'out'}},
      {'output',{ 'ch1','ch2','ch3','ch4'},{}},
   },
   edges = {
      {{'input','ch1'}, {'fir4','in1'}},
      {{'input','ch2'}, {'fir4','in2'}},
      {{'input','ch3'}, {'fir4','in3'}},
      {{'input','ch4'}, {'fir4','in4'}},
      {{'fir4','out1'}, {'output','ch1'}},
      {{'fir4','out2'}, {'output','ch2'}},
      {{'fir4','out3'}, {'eq','in1'}},
      {{'fir4','out4'}, {'eq','in2'}},
      {{'eq','out'}, {'output','ch4'}},
   },
}

-- Test for graph data structure
local function test1()
   log_desc(schematic)
   w_dot(schematic)
end



-- EXPORT

-- test1()
-- test2()
return {
   t = t,
   graph_compile = graph_compile,
   w_dot = w_dot,
   analyze = analyze,
   render_c = render_c,
   render_c_loop = render_c_loop,
   render_c_osc = render_c_osc,
   w = w,
   input_edges = input_edges,

   test = {test1, test2},

   -- FIXME: Put these elsewhere
   map = map,
   named = named,
   concat = concat,

}

