#!./lua.sh

local lxml = require('lib.lxml')
local iolist = require('lure.iolist')
require('lib.tools.log')


local function wrap_header(inner)
return {[[
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>dataflow webui</title>
  <style>
    .image-grid {
      display: grid;
      grid-template-columns: repeat(8, 1fr); /* 8 columns */
      grid-template-rows: repeat(12, 25px);
      gap: 5px;
      width: 100%;
      max-width: 1200px;
      margin: auto;
    }
    .image-grid img {
      width: 100%;
      height: 50%;
      object-fit: cover;
      display: block;
      border: 1px solid #555555
    }
  </style>
</head>
<body>
]],
inner,
[[
</body>
</html>
]]}
end



-- FIXME: Make a 'for/list' clone.  I keep doing this table mutation thing.
function inner()
   local inputs = {}
   for i=1,8*8 do
      inputs[i] =
         {'input', {
             type = 'number',
             id = 'quantity',
             min = 0,
             max = 5,
             step = 0.1,
             value = 0,
             }}
   end
   return {'div',
           {class = 'image-grid'},
           inputs}
end


local html = wrap_header(
   lxml.elements_to_string({inner()}))

local file = io.open("../webapp/dataflow/webui.html", "w")
iolist.write_to_stream(file, html)
