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

local schematic = {
   -- Input/Output name to text, keep this separate.
   block = {
      fir1 = {
      },
   },
   names = {
      ports = {},
      blocks = {},
   },

   edges = {
      {{'Input','ch1'}, {'FIR4','in1'}},
   },
}

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

local function w_graph(s)

   local input_ports = {'ch1','ch2','ch3','ch4'}



w([[
digraph G {
    graph [rankdir = LR];
    node[shape=record];
]],[[
    Input[label="{{} |
                 Input|
]],
r_ports(input_ports),
-- [[
--                  {
--                   <ch1> ch1|
--                   <ch2> ch2|
--                   <ch3> ch3|
--                   <ch4> ch4
--                  }
-- ]],
[[
                 }"];
    FIR4[label="{ {<in1>in1|
                   <in2>in2|
                   <in3>in3|
                   <in4>in4}|
                  FIR4 |
                  {<out1>out1|
                   <out2>out2|
                   <out3>out3|
                   <out4>out4} }"];

    Output[label="{ {<ch1>ch1|
                     <ch2>ch2|
                     <ch3>ch3|
                     <ch4>ch4}|
                    Output |
                    {} }"];
]],
[[
    Input:ch1 -> FIR4:in1;
    Input:ch2 -> FIR4:in2;
    Input:ch3 -> FIR4:in3;
    Input:ch4 -> FIR4:in4;
]],
[[

    FIR4:out1 -> Output:ch1;
    FIR4:out2 -> Output:ch2;

    EQ[label="{ {<in1>in1|
                 <in2>in2}|
                   EQ|
                 {<out>out} }"];

    FIR4:out3 -> EQ:in1;
    FIR4:out4 -> EQ:in2;
    EQ:out -> Output:ch4;

    

}
]])
end

w_graph(schematic)
