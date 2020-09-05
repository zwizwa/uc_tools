-- All generators are parameterized by a configuration environment
-- 'e'.  This is a little messy, but overall seems to be the simplest
-- way to do render parameterization.

-- SVG header.  This sets the style sheet and inlines a g which is
-- parameterized by environment, and expands to a group of SVG
-- commands.

local list = require('lib.tools.list')

local logsvg = {}

local function log(str) io.stderr:write(str) end

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

function logsvg.translate(x, y)
   return 'translate(' .. x .. ',' .. y .. ')'
end

function logsvg.logentry(e, time, text)
   local y = e.time_to_y(time)
   return
      {'text',
       {width=e.width,
        height='auto',
        transform=logsvg.translate(0,y),
        class='small',
        stroke='black'},
       {text}}
end

function logsvg.render(e, logs)
   local function g(e)
      local column_groups = {}
      for j, entries in ipairs(logs) do
         if e.repel then
            entries = logsvg.repel(entries, e.repel)
         end
         local x = e.column_to_x(j)
         local text_elements = {}
         for i, entry in ipairs(entries) do
            local time, text = unpack(entry)
            table.insert(text_elements, logsvg.logentry(e, time, text))
         end
         table.insert(
            column_groups,
            {'g',
             {transform=logsvg.translate(x,0)},
             text_elements})
      end
      return column_groups
   end
   return logsvg.svg(e, g)
end

-- Let the log entries repel each other, by moving them forward in
-- time if they are too close.
function logsvg.repel(entries, delta_t)
   if #entries == 0 then return entries end
   local t = entries[1][1]
   out_entries = {}
   for i, entry in ipairs(entries) do
      local time, text = unpack(entry)
      -- Update t for next entry depending on whether this one fits.
      -- FIXME: draw a line from actual time to text location.
      if time >= t then
         t = time + delta_t
      else
         time = t
         t = t + delta_t
      end
      table.insert(out_entries, {time, text})
   end
   return out_entries
end


-- Put this in a tools library, together with the popen variant.
local function read_file(filename)
   local f = io.open(filename, "r+")
   if not f then
      error("cant_open:" .. filename)
   end
   local str = f:read('*a')
   f:close()
   return str
end


-- Read a time-stamped log.  For now this assumes that the timestamp
-- is a 8-digit lower case hex number at the start of the line,
-- followed by a space.
--
-- FIXME: To handle counter wrap-around the following convention is
-- used: it is assumed that there is at least one message per
-- wrap-around (32bit, 72MHz) such that there is no aliasing.  In that
-- case, we can just catch wraps here.

function logsvg.read_log(filename)
   local str = read_file(filename)
   local lines = {}
   for stamp, logline in string.gmatch(str, "([0123456789abcdef]-) (.-)\n") do
      local n = tonumber(stamp,16)
      -- log(n .. "\n")
      -- log(logline .. "\n")
      table.insert(lines, {n, logline})
   end
   return lines
end


-- For convenience.  This is the "user scenario": convert a list of
-- microcontroller trace log files to an svg.
function logsvg.render_logfiles(e, filenames)
   return logsvg.render(e, list.map(logsvg.read_log, filenames))
end


return logsvg
