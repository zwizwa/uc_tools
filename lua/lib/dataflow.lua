#!./lua.sh

-- Generate readable dataflow schematics and patching C code from a
-- Lua-embedded 'final' DSL.


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

-- Render the patching code: structs and
local function render_c(s)
   -- Per type
   local struct_code = {}
   local process_code = {}
   local macros = {}

   -- Per instance
   local graph_struct_code = {}
   local alloc_code = {}
   local connect_code = {}
   local init_code = {}
   -- Index the edges by inputs.
   local edge = input_edges(s)
   -- log_desc({edge=edge})

   local indent = '    '
   local indent2 = {indent, indent}

   local alloc_count = 0

   local function str(n) return n .. '' end

   local did_type = {}

   -- For all processing nodes
   for _,node in ipairs(s.nodes) do
      -- Get the processor definition
      -- log_desc({node=node})
      local to_node   = node.name
      local to_ports  = node.in_ports
      local outs      = node.out_ports
      local type_name = node.type_name

      assert(to_node)
      assert(to_ports)
      assert(outs)
      assert(type_name)

      local out_struct = {}
      for _,out in ipairs(outs) do
         table.insert(
            out_struct,
            {indent2,'float *',out,';\n'})
      end
      local in_struct = {}
      for _,to_port in ipairs(to_ports) do
         -- log_desc({to_node,to_port})
         local from = edge[to_node][to_port]

         table.insert(
            in_struct,
            {indent2,'float *',to_port,';\n'})

         -- Obtain the inputs by assigning pointers.
         if from then
            assert(from)
            local from_node, from_port = unpack(from)
            local input  = {'s->', to_node,   '.input.',  to_port,   }
            local output = {'s->', from_node, '.output.', from_port, }
            table.insert(
               alloc_code, {
                  {indent, output, ' = s->buf[',str(alloc_count),']',';\n'},
            })
            alloc_count = alloc_count + 1,

            table.insert(
               connect_code, {
                  {indent, input, ' = ', output, ';\n'}
            })
         else
            -- Port is not connected.
            log_desc({not_connected={to_node,to_port}})
         end
      end

      -- Add instance to the graph struct
      table.insert(
         graph_struct_code,
         {indent, 'struct ',type_name, '_node ', to_node, ';\n'})

      -- Add instance init
      -- FIXME: Maybe better to assert(node.init) instead of allowing zero defaults.
      if node.init then
         local state = {'&s->',to_node,'.state'}
         table.insert(init_code, {indent, type_name,'_init(',state,');\n'})
         for name, value in pairs(node.init) do
            table.insert(init_code, {indent, type_name,'_set_',name,'(',state,', ',value,');\n'})
         end
      end

      -- Run the processor instance, which will put the outputs in the
      -- correct place.
      table.insert(
         process_code,
         {indent, type_name, '_loop(&s->',to_node,');\n'})

      -- Per-type struct and process only need to be done once
      assert(node.type_name)
      if not did_type[node.type_name] then
         did_type[node.type_name] = true

         -- Save the data structure
         table.insert(
            struct_code,
            {'struct ', type_name, '_node {\n',
             {indent,'struct {\n', in_struct,  indent,'} input;\n'},
             {indent,'struct {\n', out_struct, indent,'} output;\n'},
             {indent, 'struct ', type_name, '_state state;\n'},
             '};\n'})
         -- Save macros
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

   table.insert(
      graph_struct_code,
      {indent, 'float buf[',str(alloc_count),'][256];\n'});

   return {
      macros = macros,
      structs = {
         struct_code,
         {"struct graph {\n",
          -- FIXME: This should not mess up state alignment.
          {indent, "struct param_context pc;\n"},
          graph_struct_code,
          "};\n"},
      },
      connect = {'void graph_init(struct graph *s) {\n',
                 {indent, '// alloc\n'},
                 alloc_code,
                 {indent, '// connect\n'},
                 connect_code,
                 init_code,
                 '}\n'},
      process = {'void graph_process(struct graph *s) {\n', process_code, '}\n'},
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
            return {name = {sub,'.',p}, typ = 'float', offset = 1, stride = 4}
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

      -- Instances are annotated with bus_type which defines a
      -- constraint on the layout of the loop code.  E.g. when the
      -- loop code was hand coded in assembly or C to use 4 x float
      -- vectors, like FIR and biquad implementations.
      --
      -- For loop code that is generated, we can absorb transposition
      -- into the loop.

      -- TODO:
      -- . bus types need to be checked when patching things together
      -- . probably user should solve transposition in separate block if needed

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

function t.cproc_op(cproc_name, init)
   return function (c, name, nb_inputs)
      local outs = {}
      for i=1,nb_inputs do outs[i] = 'o' .. i end
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

