-- All generators are parameterized by a configuration environment
-- 'e'.  This is a little messy, but overall seems to be the simplest
-- way to do render parameterization.

-- SVG header.  This sets the style sheet and inlines a g which is
-- parameterized by environment, and expands to a group of SVG
-- commands.
local logsvg = {}

function logsvg.svg(e, g)
   assert(e)
   assert(e.width)
   assert(e.height)
   assert(g)
   return
      {'svg',
       {xmlns='http://www.w3.org/2000/svg',
        width=e.width,
        height=e.hight},{
          {'style',{},
           {".small { font-family: monospace; font-size: 10px }\n"}},
          {'g',{},g(e)}}}
end

function logsvg.translate(e, x, y)
   return 'translate(' .. x .. ',' .. y .. ')'
end

function logsvg.logentry(e, time, text)
   local y = e.time_to_y(time)
   return
      {'text',
       {width=e.width,
        height='auto',
        transform=logsvg.translate(e,0,y),
        class='small',
        stroke='black'},
       {text}}
end

function logsvg.render(entries)
   local e = {}
   e.time_to_y = function(time) return 10*time end
   e.width = 500
   e.height = 500
   local function g(e)
      local ts = {}
      for i, entry in ipairs(entries) do
         local time, text = unpack(entry)
         table.insert(ts, logsvg.logentry(e, time, text))
      end
      return ts
   end
   return logsvg.svg(e, g)
end

return logsvg
