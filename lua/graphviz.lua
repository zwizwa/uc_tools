#!./lua.sh

-- The idea is to generate readable signal flow schematics and high
-- level patching C code at the same time.

require('lib.tools.log')

-- Implementation uses Erlang style iolist to avoid string concatenation
-- iolist = listof(iolist) | string

-- The 'r_' functions will render to iolist
-- The 'w_' functions take iolist and write to stdout

local function w_iostr(s)
   if type(s) == 'string' then
      io.stdout:write(s)
   else
      for _,str in ipairs(s) do
         w_iostr(str)
      end
   end
end

local function w(...)
   w_iostr({...})
end


-- FIXME: Put these in a single file once and for all.
local function map(f,arr)
   local out_arr = {}
   for i,el in ipairs(arr) do  out_arr[i] = f(el) end
   return out_arr
end
local function join(connect_el, arr)
   local out_arr = {}
   local n = #arr
   for i,el in ipairs(arr) do 
      table.insert(out_arr, el)
      if i<n then table.insert(out_arr, connect_el) end
   end
   return out_arr
end



local function r_ports(s)
   return function (ports_list)
      local port_labels = map(function(p) return {"<",p,">",p} end, ports_list)
      local joined = join("|", port_labels)
      return {"{",joined,"}"}
   end
end

local function r_node(s)
   return function(n)
      local name, i, o = unpack(n)
      assert(name)
      assert(i)
      assert(o)
      return {
         '    ',
         name,'[label="{ ',
         r_ports(s)(i),'|',name, '|',r_ports(s)(o),
         ' }"];\n'
      }
   end
end

local function r_edge(s)
   return function(e)
      local from, to = unpack(e)
      assert(from)
      assert(to)
      local from_node, from_port = unpack(from)
      assert(from_node)
      assert(from_port)
      local to_node, to_port = unpack(to)
      assert(to_node)
      assert(to_port)
      return {
         '    ',
         from_node,':',from_port,
         ' -> ',
         to_node,':',to_port,
         ';\n'
      }
   end
end


-- https://stackoverflow.com/questions/7922960/block-diagram-layout-with-dot-graphviz
local function w_graph(s)
   assert(s.nodes)
   assert(s.edges)
   w([[
digraph G {
    graph [rankdir = LR];
    node[shape=record];
]],
map(r_node(s), s.nodes),
map(r_edge(s), s.edges),[[
}
]])
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


local function test1()
   log_desc(schematic)
   w_graph(schematic)
end

-- To keep it simple: use unique node names at first.
-- How to specify the number of outputs that a processor produces?
-- This should be part of the type definition.

local function graph_compiler()

   local c = { }

   function c:make_output(name, out_name)
      local ports = self.ports[name] or {}
      table.insert(ports, out_name)
      self.ports[name] = ports
      return {name, out_name}
   end

   function c:app(typ, name, ...)
      assert(typ)
      assert(name)

      -- The inputs are always outputs of other nodes.  What we do
      -- here is split that into explicit input port names, and
      -- explicit edges.
      local ins = {...}

      local instance = typ(self, name, #ins)
      -- log_desc({instance=instance})


      local outs = instance.outs

      local input_name = instance.input_name
      local in_port_names
      log_desc({instance=instance})
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

      -- Return just the port name.
      local out_ports = map(
         function(out)
            local _name, port = unpack(out);
            return port
         end,
         outs)

      table.insert(self.nodes, {name, in_port_names, out_ports})
      return unpack(outs)
   end


   local state = {
      -- The applicative variables are always output ports of the
      -- nodes (processor instances).
      ports = {},
      -- The Graphviz node and edge sets.
      nodes = {},
      edges = {},
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

function t.bus_op(type_name)
   return function (c, name, nb_inputs)
      local outs = {}
      for i=1,nb_inputs do outs[i] = c:make_output(name, 'o' .. i) end
      return {
         type_name = type_name,
         outs = outs,
         input_name = function(i) return 'i' .. i end
      }
   end
end

function t.matrix_op(type_name, in_names, out_names)
   return function (c, name, nb_inputs)
      local outs = {}
      for i=1,#out_names do outs[i] = c:make_output(name, out_names[i]) end
      return {
         type_name = type_name,
         outs = outs,
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
         for i=1,n do outs[i] = c:make_output(name, 'ch' .. i) end
      elseif type(n) == 'table' then
         for i,name_i in ipairs(n) do outs[i] = c:make_output(name, name_i) end
      else
         error('bad t.input type ' .. type(n))
      end
      return { outs = outs, }
   end
end

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
   w_graph(sch)

   -- FIXME: C compiler
   -- local code = c_compile(prog2)
   -- w(code)

end


-- test1()
-- test2()
return {
   t = t,
   graph_compile = graph_compile,
   w_graph = w_graph,
   test = {test1, test2},

   -- Some utility functions
   named = function(names, values)
      local tab = {}
      for i,v in ipairs(values) do
         tab[names[i]] = v
      end
      return tab
   end
}

