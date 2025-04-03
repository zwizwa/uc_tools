#!./lua.sh

-- The idea is to generate readable signal flow schematics.
-- https://stackoverflow.com/questions/7922960/block-diagram-layout-with-dot-graphviz

require('lib.tools.log')

-- This writes an Erlang style iostring

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



local function r_ports(ports_list)
   local port_labels = map(function(p) return {"<",p,">",p} end, ports_list)
   local joined = join("|", port_labels)
   return {"{",joined,"}"}
end

local function r_node(n)
   local name, i, o = unpack(n)
   assert(name)
   assert(i)
   assert(o)
   return {
      '    ',
      name,'[label="{ ',
      r_ports(i),'|',name, '|',r_ports(o),
      ' }"];\n'
   }
end

local function r_edge(e)
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


local function w_graph(s)
   assert(s.nodes)
   assert(s.edges)
   w([[
digraph G {
    graph [rankdir = LR];
    node[shape=record];
]],
map(r_node, s.nodes),
map(r_edge, s.edges),[[
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

local c = { }
function c:node()
   local n = self.n + 1
   self.n = n
   return n
end
function c:app(typ, name, ...)
   local ins = {...}
   assert(typ)
   assert(name)
   local outs = typ(self, name, ins)
   table.insert(self.nodes, {name, ins, outs})
   return unpack(outs)
end
local function compiler()
   local state = {
      n = 0,
      nodes = {},
      edges = {},
   }
   setmetatable(state, {__index = c})
   return state
end
local function compile(prog)
   local c = compiler()
   local ins = {c:node()}
   local outs = prog(c, unpack(ins))
   return c
end

local function test2()
   function t_filter(c, name, ins)
      assert(type(name) == 'string')
      assert(type(ins) == 'table')
      local outs = map(function(i) return c:node() end, ins)
      return outs
   end
   local function prog(c, i)
      local a = c:app(t_filter, 'f1', i )
      local b = c:app(t_filter, 'f2', i, a )
      return b
   end
   local sch = compile(prog)
   log_desc(sch)
   -- w_graph(sch)
end


-- test1()
test2()


