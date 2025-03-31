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
   return {
      '    ',n.name,'[label="{ ',
      r_ports(n.i),'|',n.name, '|',r_ports(n.o),
      ' }"];\n'
   }
end

local function w_graph(s)
   w([[
digraph G {
    graph [rankdir = LR];
    node[shape=record];
]],
r_node(s.input),
r_node(s.fir4),[[
    input:ch1 -> fir4:in1;
    input:ch2 -> fir4:in2;
    input:ch3 -> fir4:in3;
    input:ch4 -> fir4:in4;
]],
r_node(s.eq),
r_node(s.output),[[
    fir4:out1 -> output:ch1;
    fir4:out2 -> output:ch2;
    fir4:out3 -> eq:in1;
    fir4:out4 -> eq:in2;
    eq:out -> output:ch4;
}
]])
end


local schematic = {
   input = {
      name = 'input',
      i = {},
      o = { 'ch1','ch2','ch3','ch4' },
   },
   fir4 = {
      name = 'fir4',
      i = { 'in1','in2','in3','in4' },
      o = { 'out1','out2','out3','out4' },
   },
   eq = {
      name = 'eq',
      i = { 'in1','in2'},
      o = { 'out'},
   },
   output = {
      name = 'output',
      i = { 'ch1','ch2','ch3','ch4' },
      o = {},
   },
   edges = {
      {{'input','ch1'}, {'fir4','in1'}},
   },
}
w_graph(schematic)
